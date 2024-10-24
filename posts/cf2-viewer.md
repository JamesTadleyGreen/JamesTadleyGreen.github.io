---
title: CF2 Viewer
tags: work, cf2, pdf
---

# The problem
At work, we need to view `.cf2` files. It turns out this might be a slightly esoteric 
file type, as I couldn't find a working viewer online, and the only python module that 
came close to viewing the file was [Aspose.CAD](https://pypi.org/project/aspose-cad/).
Unfortunately, this option was not fit for use for us. Firstly, it's copywritten.
Secondly, even if it wasn't, it applies a watermark to the file.

> So, how do we programmatically view `.cf2` files?

I guess we have to write it ourselves.

# Exploring the problem
## The file
The first piece of good news is that `.cf2` files are plaintext, meaning we can open
them in notepad (or similar) and view the contents of the file. 

The other piece of good news is the file is kinda readable. We can have a first stab
at what the lines might mean without knowing _any_ specification.

An example file is below:

```
$BOF
V2
ORDER
END
MAIN,Layout
UM
LL,-155.526,183.405,
UR,1419.474,1250.405,
SCALE,1,1,
C,AGD,-155.526,183.405,0,1,1
END
SUB,AGD
L,2,1,0,34.500,74.500,34.500,222.500,0,0.000
L,2,1,0,34.500,222.500,244.500,222.500,0,0.000
L,2,1,0,244.500,222.500,244.500,74.500,0,0.000
L,2,1,0,244.500,74.500,34.500,74.500,0,0.000
END
$EOF
```

## The Specification
Crucially, I found a sample (although incomplete) specification on 
[this forum.](https://forums.autodesk.com/t5/autocad-forum/is-there-a-way-to-write-a-command-to-convert-or-save-autocad/td-p/12748340)
I have no idea how this was from this year, as every other forum post is from a decade
ago. Incredibly lucky!

As I said above, the spec isn't complete and has some errors. I've managed to figure
these out through some trial and error.

### Filling in the gaps
The hardest part for me to figure out was arcs, you can see the specification is not 
complete, stating that the structure is of the form:
```
A, p, t, at, sx, sy, ex, ey, cx, cy, =/-1, nbridges, wbridges
```

But, gives no explanation for what `cx`, `cy`, or `=/-1` means.

It turns out, to describe an arc, it represents:
 - `A` meaning arc
 - `p` representing the pointage of the line
 - `t` representing the line style
 - `at` used for arrows
 - `sx` the `x` start coordinate for the arc
 - `sy` the `y` start coordinate for the arc
 - `ex` the `x` end coordinate for the arc
 - `ey` the `y` end coordinate for the arc
 - `cx` the `x` coordinate representing the centre of the circle the arc lies upon
 - `cy` the `y` coordinate representing the centre of the circle the arc lies upon
 - `=/-1` the direction for the arc to be drawn (clockwise or anticlockwise)
 - `nbridges` the number of bridges in the line.
 - `wbridges` the width of the bridges in mm.

### Still unknowns
The spec doesn't go into enough detail, and I don't have enough example files for all 
of the file's logic.

For example; there is a parameters file, that should allow client information to be read
into the file. I haven't even starting looking at how this would work.

Also, arrows on lines, this uses the auxiliary line style, again I haven't starting
looking at this.

Finally, scale, I've starting using this parameter. But, I don't have any example files
to test my expected output against, so it's very incomplete / not implemented.

# The solution
## CF2 Parser
### CF2 class
This is the meat of the module. I've structured the module and logic taking inspiration
from `pandas` `DataFrame`. As such, you can either parse a `cf2` file directly, or 
construct one from the ground up. Also, you have the ability to combine `cf2`s. At some
point I'll add an equality or difference measure as well.

The `CF2` class is of the structure:
```python
class CF2:
    def __init__(
        self,
        dimensions: tuple[tuple[int, int], tuple[int, int]],
        parameters: dict | None = None,
        scale: tuple[int, int] = (1, 1),
        routine: list[Line, Arc, Text, SubroutineCall] | None = None,
        subroutines: list[Subroutine] | None = None,
    ):
        self.dimensions = dimensions
        self.parameters = parameters if parameters else {}
        self.scale = scale
        self.routine = routine if routine else []
        self.subroutines = subroutines if subroutines else []
```
With sensible defaults.

The key method, of this class, is the `flatten` method, this converts subroutines to a 
'flat' routine. Meaning instead of any subroutine calls, we instead have a super long
list of instructions.
```python
def flatten(self) -> list[Line, Arc, Text]:
    output = []
    for subroutine in self.routine:
        if isinstance(subroutine, SubroutineCall):
            for instruction in [
                i for i in self.subroutines if i.name == subroutine.name
            ][0].instructions:
                output.append(instruction.adjust(subroutine))
        else:
            output.append(subroutine)
    return output
```

The `isinstance` check, checks if the 'instruction' in a `routine` is a `SubroutineCall`,
or is instead an instance of `Line`, `Arc`, or `Text`. 'Flattening' replaces those
`SubroutineCalls` with their 'instructions' (of type `Line`, `Arc`, or `Text`).

### Lines, arcs, and text
All of these classes have similar methods, at some point I'll move them to be subclassed
from a more general parent. At this current moment in time, one can consider these 
objects to be of the form:
```python
class Object(*args):
    def __init__():
        self.*args = *args

    def adjust(*args):
        handle_translation()
        handle_rotation()
        handle_scaling()
```

### Subroutines
There are two confusingly similar concepts here, the `Subroutine` and the
`SubroutineCall`, these differ as below:
 - `Subroutine`: A list of instructions of the form `Line`, `Arc`, or `Text`, with an 
 associated name. It's not in the CF2 specification, but my construction allows for
 subroutines to call other subroutines with a `SubroutineCall`, this isn't allowed in 
 the spec.
 - `SubroutineCall`: A call within the `Routine` defining at what point to execute a 
 named `Subroutine`

### Adjust
The adjust logic for each class handles translation, rotation, and scaling. For each
of the classes this is slightly different albeit very related. For example, `Arc`s need
to also move their centre point when translating, but `Line`s don't have this as a 
property.

## PDF output
### Lines, arcs, and text
There are corresponding functions in `pycairo` for each of these classes.

Line is very simple, we simply `move_to` the start of the line, and `line_to` the 
endpoint.

Text is also simple, we `move_to` the location, `rotate` by the angle given, do a negative
1 scaling in the y-axis (due to `pycairo`'s [reversal of the axis](#dimensions) compared 
to the CF2 spec). Then revert this scaling once the text is placed. The size of text 
will be implemented in a future release.

Arc is the only slightly difficult case, `pycairo` uses polar coordinates, whereas
the CF2 spec uses the start, end, and centre of circle. So we need to convert this,
using the below:
```python
def convert_to_polar(arc: Arc) -> tuple[float, float, float]:
    radius = calculate_radius(arc.centre, arc.start)
    start_angle = math.pi + calculate_angle(
        arc.centre, arc.start if arc.direction == 1 else arc.end)
    end_angle = math.pi + calculate_angle(
        arc.centre, arc.end if arc.direction == 1 else arc.start)
    return start_angle, end_angle, radius
```

Then we have a final edge case where the arc is a circle, and we're done!

### Dimensions
To define the size of the artboard for the cutter we have the lower left and the 
upper right defined in the CF2 file. So we set the size of the PDF to be the difference
of these two. Then translate the x and y-axes by the lower left coordinate (remembering
the y-axis is reversed).

Finally we invert the matrix `pycairo` uses, to align our axes.
```python
context.set_matrix(cairo.Matrix(1, 0, 0, -1))
```

### Colours and styles
Currently, we set sensible defaults for our colours, in the future I'll add the ability
to set one's own colours via the command line. We have a very simple `dict` set up:
```python
LINE_TYPES = {
    LineType.CUT: {"rgb": (1, 0, 1), "dash": None},
    LineType.CREASE: {"rgb": (0, 1, 1), "dash": [10, 5]},
    LineType.PERF: {"rgb": (0, 0.75, 0), "dash": [2, 2]},
    LineType.SCORE: {"rgb": (0.75, 0.75, 0), "dash": [2, 2]},
    LineType.UNKNOWN: {"rgb": (1, 0, 0), "dash": None},
}
```
The  line width is set based upon the pointage defined for each instruction.

## Writing CF2s
If we add to the CF2, or amend any of the existing geometry, we want to be able to 
write the new CF2 to a file for further use.

### `__repr__`
This is the key functionality, if we give every class a sensible `__repr__`, namely the
representation of a line in the CF2, then, to output to CF2, all we do is write the 
`__repr__` to each class in the correct order.

For example, the `__repr__` of an `Arc` is:
```python
def __repr__(self):
    return f"A,{self.pointage},{self.linetype.value},0,{self.start[0]},{self.start[1]},{self.end[0]},{self.end[1]},{self.centre[0]},{self.centre[1]},{self.direction},{self.nbridges},{self.wbridges}"
```

This looks little like confusing and a little gross, but it simplifies the remainder of 
our code.
Let's look at a `Subroutine`:
```python
def __repr__(self):
    return "\n".join(
        [f"SUB,{self.name}"] + [i.__repr__()
                                for i in self.instructions] + ["END"]
    )
```
This creates a lot of lines, starting with `SUB`, and ending with `END`. In-between these
values, we have the representation of each of the instructions (`Line`, `Arc`, or `Text`). 
This compartmentalisation of each class allows very compact code.

### Sensible defaults
In the `CF2` class we have some defaults set with `None`:
```python
class CF2:
    def __init__(
        self,
        dimensions: tuple[tuple[int, int], tuple[int, int]],
        parameters: dict | None = None,
        scale: tuple[int, int] = (1, 1),
        routine: list[Line, Arc, Text, SubroutineCall] | None = None,
        subroutines: list[Subroutine] | None = None,
    ):
        self.dimensions = dimensions
        self.parameters = parameters if parameters else {}
        self.scale = scale
        self.routine = routine if routine else []
        self.subroutines = subroutines if subroutines else []
```

The reason for this is, if we set them to be the empty `list` and the empty `dict`, then
on subsequent constructions of the class, they share the same object.

What this means in reality, is, when you create multiple CF2s (e.g. parsing over a 
folder of CF2s), when you add a subroutine to the first `CF2`, it'll be added to all
`CF2`s.

> I did know this, and I still fell into this trap and had to bugfix my way out!

[This article](https://www.pullrequest.com/blog/python-pitfalls-the-perils-of-using-lists-and-dicts-as-default-arguments/#:~:text=However%2C%20the%20problem%20lies%20with,t%20provide%20an%20item%20list.)
covers this issue in much greater detail than I could.
To me, this highlights the importance of reading around your language / knowledge area.
I read about this 'quirk' of Python for the first time probably about 5 years ago,
and whilst I still fell into the trap, it was a much quicker bugfix knowing this might 
be an issue rather than discovering it for myself.

# Conclusion
This was a fun little weekend project, the `__repr__` felt like the cleanest piece of 
code that I did. Moving forward I'll add functionality for some of the stuff in the 
specification I've missed (e.g. scaling and text size). I'll also probably improve the 
interface for updating the CF2s, adding in functionality for adding and amending 
subroutines.
