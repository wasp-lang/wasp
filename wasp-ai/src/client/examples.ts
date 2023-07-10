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
    description: `A blogging platform with posts and post comments.
User owns posts and comments and they are saved in the database.
Everybody can see all posts, but only the owner can edit or delete them. Everybody can see all the comments.
App has four pages:
1. "Home" page lists all posts (their titles and authors) and is accessible by anybody.
   If you click on a post, you are taken to the "View post" page.
   It also has a 'New post' button, that only logged in users can see, and that takes you to the "New post" page.
2. "New post" page is accessible only by the logged in users. It has a form for creating a new post (title, content).
3. "Edit post" page is accessible only by the post owner. It has a form for editing the post with the id specified in the url.
4. "View post" page is accessible by anybody and it shows the details of the post with the id specified in the url: its title, author, content and comments.
    It also has a form for creating a new comment, that is accessible only by the logged in users.
`,
    color: availableColors.find((color) => color.name === "amber"),
    complexity: "complex",
  }
];
