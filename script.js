const burger = document.querySelector(".burger");
const pages = document.querySelector(".pages");

burger.addEventListener("click", () => {
  burger.classList.toggle("active");
  pages.classList.toggle("active");
});

document.querySelectorAll(".control").forEach((n) =>
  n.addEventListener("click", () => {
    burger.classList.remove("active");
    pages.classList.remove("active");
  })
);
