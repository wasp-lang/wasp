import { availableColors } from "./components/Color";

export const poolOfExampleIdeas = [
  {
    name: "TodoApp",
    description:
      "A simple todo app with one main page that lists all the tasks. I can create new tasks, or toggle existing ones. " +
      "User owns tasks. User can only see and edit their own tasks. Tasks are saved in the database.",
    color: availableColors.find((color) => color.name === "rose"),
  },
  {
    name: "Blog",
    description:
      "A blog with posts and comments. Posts can be created, edited and deleted. Comments can be created and deleted. Posts and comments are saved in the database.",
    color: availableColors.find((color) => color.name === "amber"),
  },
  {
    name: "FlowerShop",
    description:
      "A flower shop with a main page that lists all the flowers. I can create new flowers, or 'water' existing ones. " +
      "User owns flowers. User can only see and edit their own flowers. Flowers are saved in the database.",
    color: availableColors.find((color) => color.name === "emerald"),
  },
  {
    name: "WeatherApp",
    description:
      "Build a simple app that uses the browser to get the current user's location. It then fetches the current weather. It uses the Open Weather API on the backend",
    color: availableColors.find((color) => color.name === "sky"),
  },
];
