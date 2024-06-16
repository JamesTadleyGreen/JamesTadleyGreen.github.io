// https://stackoverflow.com/questions/66852102/css-scroll-snap-get-active-item

let role = document.getElementById("role-selector");
let interest = document.getElementById("interest-selector");
let currentItem = null;

// Store items as an array of objects
const items = Array.from(role.children).map((el) => el.textContent);

const detectCurrent = () => {
  const scrollY = role.scrollTop; // role scroll position
  console.log(scrollY);
  const goal = Math.round(scrollY / 35);

  // Find item closest to the goal
  currentItem = items[goal];

  interests = {
    Recruiter: ["Job History", "Your CV", "Manager Experience"],
    "Maths Person": ["Advent of Code", "Code Golf", "Applications of Maths"],
    "Comp Sci Person": ["Code Golf", "API design"],
    "Data Scientist": ["Applications of Data", "Dashboarding"],
    Artist: ["How to design a dashboard"],
  };
  // Add children to the interest selector
  interest.innerHTML = "";
  for (i = 0; i < interests[currentItem].length; i++) {
    interest_node = document.createElement("div");
    interest_node.innerHTML = interests[currentItem][i];
    interest.appendChild(interest_node);
  }
};

role.addEventListener("scroll", () => detectCurrent()); // Detect current item on scroll
