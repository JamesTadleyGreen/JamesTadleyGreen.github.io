// https://stackoverflow.com/questions/66852102/css-scroll-snap-get-active-item

let role = document.getElementById("role-selector");
let interest = document.getElementById("interest-selector");
let currentItem = null;

const interests = {
  Recruiter: ["Job History", "Your CV", "Manager Experience"],
  Mathematician: ["Advent of Code", "Code Golf", "Applications of Maths"],
  "Computer Scientist": ["Code Golf", "API design"],
  "Data Scientist": ["Applications of Data", "Dashboarding"],
  Artist: ["How to design a dashboard"],
};

// Store items as an array of objects
const items = Array.from(role.children).map((el) => el.textContent);

const detectCurrent = () => {
  const scrollY = role.scrollTop; // role scroll position
  console.log(scrollY);
  const goal = Math.round((scrollY - 8) / 83);

  console.log(goal);

  // Find item closest to the goal
  currentItem = items[goal];
  console.log(currentItem);

  // Add children to the interest selector
  interest.innerHTML = "";
  for (i = 0; i < interests[currentItem].length; i++) {
    interest_node = document.createElement("div");
    interest_node.innerHTML = interests[currentItem][i];
    interest.appendChild(interest_node);
  }
};

role.addEventListener("scroll", () => detectCurrent()); // Detect current item on scroll
[role, interest].forEach((el) =>
  el.addEventListener("wheel", () => resetScroll(el)),
);
const resetScroll = (el) => {
  el.addEventListener("wheel", function (event) {
    if (event.deltaY === 100 || event.deltaY === -100) {
      event.preventDefault();
      el.scrollBy({
        top: 50 * Math.sign(event.deltaY),
        behavior: "smooth",
      });
    }
  });
};
