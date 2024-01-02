import { useMemo } from "react";
import { validProjectBrandColors } from "./Color";
import Tilt from "react-parallax-tilt";

const NUM_APPS_WE_CAN_GENERATE_IN_PARALLEL = 7;
const AVG_APP_GENERATION_TIME_IN_MIN = 3;

export function WaitingRoomContent(props) {
  const estimatedWaitingTimeInMinutes = useMemo(() => {
    return Math.max(
      1,
      Math.round(
        ((props.numberOfProjectsAheadInQueue - 1) /
          NUM_APPS_WE_CAN_GENERATE_IN_PARALLEL) *
          AVG_APP_GENERATION_TIME_IN_MIN
      )
    );
  }, [props.numberOfProjectsAheadInQueue]);

  return (
    <>
      <header className="relative mb-4 bg-slate-50 p-8 rounded-xl text-gray-500">
        {estimatedWaitingTimeInMinutes > 5 && (
          <>
            <p className="bg-yellow-100 text-center text-yellow-700 rounded p-2">
              🚨 We are experiencing heavy load at the moment! 🚨
            </p>
            <br />
          </>
        )}
        <p className="text-gray-500">Hello there! 🤖</p>
        <br />
        <p>
          Thanks for trying out the AI web App Generator. Looks like it might be
          a bit before we can start building your app.
        </p>
        <br />
        <p>
          Estimated waiting time:{" "}
          <strong>{estimatedWaitingTimeInMinutes} minutes.</strong>
        </p>
        <br />
        <p>
          In the meantime, you can: <br />
          1.{" "}
          <strong>
            Take a look at some of our already generated apps below.
          </strong>{" "}
          You can check out the real generated code, and also try downloading
          and running them.
          <br />
          2. Read more about the GPT Web App Generator{" "}
          <a href="#faq" className="underline">
            here
          </a>
          .<br />
          3. Join our{" "}
          <a
            href="https://discord.gg/rzdnErX"
            className="underline"
            target="_blank"
            rel="noopener noreferrer"
          >
            Discord
          </a>{" "}
          and chat with us about the project.
          <br />
        </p>
        <br />
        <p>
          Come back to this page to check the status of your app at any time!
        </p>
      </header>
      <h3 className="text-xl font-semibold mb-4 text-slate-800">
        Examples of already generated apps:
      </h3>

      <div className="grid grid-cols-1 gap-2 lg:grid-cols-2 lg:gap-4">
        {showcaseSamples.map((sample) => (
          <ShowcaseCard key={sample.name} {...sample} />
        ))}
      </div>
    </>
  );
}

export const showcaseSamples = [
  {
    name: "TodoApp",
    specimenURL:
      "https://usemage.ai/result/07ed440a-3155-4969-b3f5-2031fb1f622f",
    description:
      "A simple todo app with one main page that lists all the tasks." +
      " User can create new tasks by providing their description, toggle existing ones, or edit their description." +
      " User owns tasks. User can only see and edit their own tasks. Tasks are saved in the database.",
    color: validProjectBrandColors.find((color) => color.name === "sky"),
    complexity: "simple",
  },
  {
    name: "MyPlants",
    specimenURL:
      "https://usemage.ai/result/3bb5dca2-f134-4f96-89d6-0812deab6e0c",
    description:
      "An app where user can track their plants and their watering schedule." +
      "\nHome page lists all of the user's plants. For each plant, number of days left until it needs to be watered is shown, as well as the plant's name, and a 'Water' button." +
      " Home page also has a 'Add plant' button, that takes you to a new page where user can create a new plant." +
      "\nWhen creating a new plant, user should give it a name and specify how often it needs to be watered (in the number of days)." +
      " Plants are saved in the database. User can access only their own plants.",
    color: validProjectBrandColors.find((color) => color.name === "green"),
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
            <button className="button sm gray">Check out the code</button>
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
