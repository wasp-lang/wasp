import waspLogo from "../waspLogo.png";
import { useState, useMemo } from "react";
import startGeneratingNewApp from "@wasp/actions/startGeneratingNewApp";
import getAppGenerationResult from "@wasp/queries/getAppGenerationResult";
import { useQuery } from "@wasp/queries";
import { CodeHighlight } from "../components/CodeHighlight";

const MainPage = () => {
  const [appName, setAppName] = useState("");
  const [appDesc, setAppDesc] = useState("");
  const [appId, setAppId] = useState("");
  const [generationDone, setGenerationDone] = useState(false);
  const { data: appGenerationResult } = useQuery(
    getAppGenerationResult,
    { appId },
    { enabled: !!appId && !generationDone, refetchInterval: 3000 }
  );
  const [activeFilePath, setActiveFilePath] = useState(null);

  if (
    appGenerationResult?.status === "success" ||
    appGenerationResult?.status === "failure"
  ) {
    if (!generationDone) {
      setGenerationDone(true);
    }
  }

  const logs = appGenerationResult?.messages
    .filter((m) => m.type === "log")
    .map((m) => m.text);
  let files = {};
  {
    appGenerationResult?.messages
      .filter((m) => m.type === "write-file")
      .map((m) => m.text.split("\n"))
      .forEach(([path, ...contentLines]) => {
        files[path] = contentLines.join("\n");
      });
  }

  function fillInExampleAppDetails() {
    setAppName("TodoApp");
    setAppDesc(
      "A simple todo app with one main page that lists all the tasks. I can create new tasks, or toggle existing ones." +
        "User owns tasks. User can only see and edit their own tasks. Tasks are saved in the database."
    );
  }

  async function startGenerating(event) {
    event.preventDefault();
    if (!(appName && appDesc)) {
      return alert("Please enter an app name and description.");
    }
    setAppId(await startGeneratingNewApp({ appName, appDesc }));
  }

  const language = useMemo(() => {
    if (activeFilePath) {
      const ext = activeFilePath.split(".").pop();
      if (["jsx", "tsx", "js", "ts"].includes(ext)) {
        return "javascript";
      } else if (["wasp"].includes(ext)) {
        return "wasp";
      } else {
        return ext;
      }
    }
  }, [activeFilePath]);

  return (
    <div className="container">
      <div
        className="
        flex
        justify-flex-start
        items-center
        mb-8
      "
      >
        <img src={waspLogo} alt="wasp" className="w-16" />
        <h1
          className="
        text-3xl
        font-bold
        text-gray-800
        ml-4
      "
        >
          Wasp AI App Generator
        </h1>
      </div>

      <form onSubmit={startGenerating}>
        <div className="mb-4 flex flex-col gap-2">
          <input
            required
            type="text"
            placeholder="Your app name"
            value={appName}
            onChange={(e) => setAppName(e.target.value)}
            disabled={appId}
          />
          <textarea
            required
            placeholder="Input for the AI on what your app should do"
            value={appDesc}
            rows="5"
            cols="50"
            onChange={(e) => setAppDesc(e.target.value)}
            disabled={appId}
          />
        </div>
        <button className="button" disabled={appId}>
          Generate
        </button>
        <div className="mt-2">
          <button
            type="button"
            disabled={appId}
            onClick={() => fillInExampleAppDetails()}
            className="button gray"
          >
            Fill in with example app details
          </button>
        </div>
      </form>

      {appId && !generationDone && (
        <div className="mt-8 mb-4 p-4 bg-slate-100 rounded">Generating...</div>
      )}

      {logs && logs.length > 0 && (
        <>
          <h2
            className="
          text-xl
          font-bold
          text-gray-800
          mt-8
          mb-4
        "
          >
            Logs
          </h2>
          <div className="flex flex-col gap-2">
            {logs.map((log, i) => (
              <pre key={i} className="p-4 bg-slate-100 rounded">
                {log}
              </pre>
            ))}
          </div>
        </>
      )}

      {files && Object.keys(files).length > 0 && (
        <>
          <h2
            className="
          text-xl
          font-bold
          text-gray-800
          mt-8
          mb-4
        "
          >
            Files
          </h2>
          <div className="grid gap-4 grid-cols-[300px_minmax(900px,_1fr)_100px]">
            <aside className="bg-slate-100 p-4 rounded flex flex-col gap-2 sticky top-0">
              {Object.keys(files).map((path) => (
                <div
                  key={path}
                  className={
                    "px-4 py-2 bg-slate-200 rounded cursor-pointer " +
                    (activeFilePath === path ? "bg-yellow-400" : "")
                  }
                  onClick={() => setActiveFilePath(path)}
                >
                  <div className="font-bold">{path}</div>
                </div>
              ))}
            </aside>

            {activeFilePath && (
              <main className="flex flex-col gap-4">
                <div
                  key={activeFilePath}
                  className="px-4 py-2 bg-slate-100 rounded"
                >
                  <div className="font-bold">{activeFilePath}:</div>  
                  <CodeHighlight language={language}>
                    {files[activeFilePath]}
                  </CodeHighlight>
                </div>
              </main>
            )}
          </div>
        </>
      )}
    </div>
  );
};
export default MainPage;
