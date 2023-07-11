import { availableColors } from "./Color";
import Tilt from "react-parallax-tilt";

export function WaitingRoomContent() {
  return (
    <>
      <header className="relative mb-4 bg-slate-50 p-8 rounded-xl text-gray-500">
        <p className="text-gray-500">Hello there! 🤖</p>
        <br />
        <p>
          Thanks for trying out the AI web App Generator. Looks like it might be
          a couple of minutes before we can start building your app. In the
          meantime, you can take a look at some of our already generated apps
          below.
        </p>
        <br />
        <p>
          Come back to this page to check the status of your app at any time!
        </p>
      </header>
      <h3 class="text-xl font-semibold mb-4 text-slate-800">
        Examples of already generated apps:
      </h3>
      <div className="flex flex-row items-stretch gap-x-10">
        {showcaseSamples.map((sample) => (
          <ShowcaseCard {...sample} />
        ))}
      </div>
    </>
  );
}

export const showcaseSamples = [
  {
    name: "TodoApp",
    specimenURL:
      "https://magic-app-generator.wasp-lang.dev/result/07ed440a-3155-4969-b3f5-2031fb1f622f",
    description:
      "A simple todo app with one main page that lists all the tasks." +
      " User can create new tasks by providing their description, toggle existing ones, or edit their description." +
      " User owns tasks. User can only see and edit their own tasks. Tasks are saved in the database.",
    color: availableColors.find((color) => color.name === "sky"),
    complexity: "simple",
  },
  {
    name: "MyPlants",
    specimenURL:
      "https://magic-app-generator.wasp-lang.dev/result/3bb5dca2-f134-4f96-89d6-0812deab6e0c",
    description:
      "An app where user can track their plants and their watering schedule." +
      "\nHome page lists all of the user's plants. For each plant, number of days left until it needs to be watered is shown, as well as the plant's name, and a 'Water' button." +
      " Home page also has a 'Add plant' button, that takes you to a new page where user can create a new plant." +
      "\nWhen creating a new plant, user should give it a name and specify how often it needs to be watered (in the number of days)." +
      " Plants are saved in the database. User can access only their own plants.",
    color: availableColors.find((color) => color.name === "green"),
    complexity: "moderate",
  },
];

export function ShowcaseCard({ name, specimenURL, description, color }) {
  return (
    <Tilt
      tiltMaxAngleX={10}
      tiltMaxAngleY={10}
      perspective={1000}
      transitionSpeed={1000}
      scale={1.05}
    >
      <div
        className="h-full bg-slate-50 p-8 rounded-xl mt-2 flex flex-col items-center cursor-pointer hover:shadow-lg transition-all"
        onClick={() => {
          window.open(specimenURL, "_blank");
        }}
      >
        <div className="idea">
          <div className="flex justify-between items-center mb-4">
            <h4 className="text-xl font-semibold text-slate-700 mb-1">
              <span
                className="inline-block w-4 h-4 rounded-full mr-2"
                style={{ backgroundColor: color.color }}
              ></span>
              {name}
            </h4>
            <button className="button sm gray">See how it looks</button>
          </div>
          <div className="text-base leading-relaxed text-slate-500 line-clamp-[10]">
            {description.split("\n").map((str) => (
              <p>{str}</p>
            ))}
          </div>
        </div>
      </div>
    </Tilt>
  );
}
