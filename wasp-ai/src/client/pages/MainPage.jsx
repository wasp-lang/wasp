import { useState } from "react";
import startGeneratingNewApp from "@wasp/actions/startGeneratingNewApp";
import { StatusPill } from "../components/StatusPill";
import { useHistory } from "react-router-dom";
import { Title } from "../components/Title";

const MainPage = () => {
  const [appName, setAppName] = useState("");
  const [appDesc, setAppDesc] = useState("");
  const [currentStatus, setCurrentStatus] = useState({
    status: "idle",
    message: "Waiting for instructions",
  });
  const history = useHistory();

  async function startGenerating(event) {
    event.preventDefault();
    if (!(appName && appDesc)) {
      return alert("Please enter an app name and description.");
    }
    setCurrentStatus({
      status: "inProgress",
      message: "Booting up AI",
    });
    const appId = await startGeneratingNewApp({ appName, appDesc });
    history.push(`/result/${appId}`);
  }

  const exampleIdeas = [
    {
      name: "TodoApp",
      description:
        "A simple todo app with one main page that lists all the tasks. I can create new tasks, or toggle existing ones." +
        "User owns tasks. User can only see and edit their own tasks. Tasks are saved in the database.",
    },
    {
      name: "Blog",
      description:
        "A blog with posts and comments. Posts can be created, edited and deleted. Comments can be created and deleted. Posts and comments are saved in the database.",
    },
    {
      name: "Flower Shop",
      description:
        "A flower shop with a main page that lists all the flowers. I can create new flowers, or toggle existing ones." +
        "User owns flowers. User can only see and edit their own flowers. Flowers are saved in the database.",
    },
  ];

  function useIdea(idea) {
    setAppName(idea.name);
    setAppDesc(idea.description);
    window.scrollTo(0, 0);
  }

  return (
    <div className="container">
      <div className="mb-4 bg-slate-50 p-8 rounded-xl flex justify-between items-center">
        <Title />
        <StatusPill status={currentStatus.status}>
          {currentStatus.message}
        </StatusPill>
      </div>

      <form onSubmit={startGenerating} className="bg-slate-50 p-8 rounded-xl">
        <div className="mb-4 flex flex-col gap-2">
          <input
            required
            type="text"
            placeholder="Your app name"
            value={appName}
            onChange={(e) => setAppName(e.target.value)}
            disabled={currentStatus.status !== "idle"}
          />
          <textarea
            required
            placeholder="Input for the AI on what your app should do"
            value={appDesc}
            rows="5"
            cols="50"
            onChange={(e) => setAppDesc(e.target.value)}
            disabled={currentStatus.status !== "idle"}
          />
        </div>
        <button
          className="button mr-2"
          disabled={currentStatus.status !== "idle"}
        >
          Engage the AI
        </button>
      </form>
      <div className="mt-8">
        <h3 className="text-xl font-semibold mb-4">Some example ideas</h3>
        {exampleIdeas.map((idea) => (
          <div
            key={idea.name}
            className="bg-slate-50 p-8 rounded-xl mt-2 flex items-center"
          >
            <div className="idea">
              <h4 className="text-lg font-semibold text-slate-700 mb-1">
                {idea.name}
              </h4>
              <p className="text-base leading-relaxed text-slate-500">
                {idea.description}
              </p>
            </div>
            <div className="flex-shrink-0 ml-12">
              <button className="button gray" onClick={() => useIdea(idea)}>
                Use this idea
              </button>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
};
export default MainPage;
