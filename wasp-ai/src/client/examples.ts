import { availableColors } from "./components/Color";

export const exampleIdeas = [
  {
    name: "TodoApp",
    description:
      "A simple todo app with one main page that lists all the tasks." +
      " User can create new tasks by providing their description, toggle existing ones, or edit their description." +
      " User owns tasks. User can only see and edit their own tasks. Tasks are saved in the database.",
    color: availableColors.find((color) => color.name === "sky"),
    complexity: "simple",
  },
  {
    name: "MyPlants",
    description:
      "An app where user can track their plants and their watering schedule."
      + "\nHome page lists all of the user's plants. For each plant, number of days left until it needs to be watered is shown, as well as the plant's name, and a 'Water' button."
      + " Home page also has a 'Add plant' button, that takes you to a new page where user can create a new plant."
      + "\nWhen creating a new plant, user should give it a name and specify how often it needs to be watered (in the number of days)."
      + " Plants are saved in the database. User can access only their own plants.",
    color: availableColors.find((color) => color.name === "green"),
    complexity: "moderate",
  },
  {
    name: "Blog",
    description:
      "A blog with posts and post comments. Posts can be created, edited and deleted. Comments can be created. Posts and comments are saved in the database."
      + "\nUser owns posts and comments. Everybody can see all posts and comments, but only the owner can edit or delete them."
      + "\nHome page lists all posts (their titles and authors) and is accessible by anybody. It also has a 'New post' button, that only logged in users can see."
      + "\nThere is a second page, to which 'New post' button takes you, for creating / editing a post. Only post author can access it."
      + "\nFinally, there is a third page, for viewing individual post in details, to which you are taken by clicking on the post on the home page."
      + "\nAnybody can access this page. It also lists all post comments, and logged-in users can add new comments."
      ,
    color: availableColors.find((color) => color.name === "amber"),
    complexity: "complex",
  }
];
