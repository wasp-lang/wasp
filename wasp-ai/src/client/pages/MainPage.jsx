import waspLogo from "../waspLogo.png";
import { useState, useMemo } from "react";
import startGeneratingNewApp from "@wasp/actions/startGeneratingNewApp";
import getAppGenerationResult from "@wasp/queries/getAppGenerationResult";
import { useQuery } from "@wasp/queries";
import { CodeHighlight } from "../components/CodeHighlight";
import { FileTree } from "../components/FileTree";
import { Loader } from "../components/Loader";

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
    .map((m) => m.text)
    .reverse();

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

  const interestingFilePaths = useMemo(() => {
    if (files) {
      return Object.keys(files)
        .filter(
          (path) =>
            path !== ".env.server" &&
            path !== ".env.client" &&
            path !== "src/client/vite-env.d.ts" &&
            path !== "src/client/tsconfig.json" &&
            path !== "src/server/tsconfig.json" &&
            path !== "src/shared/tsconfig.json" &&
            path !== ".gitignore" &&
            path !== "src/.waspignore" &&
            path !== ".wasproot"
        )
        .sort(
          (a, b) =>
            (a.endsWith(".wasp") ? 0 : 1) - (b.endsWith(".wasp") ? 0 : 1)
        );
    } else {
      return [];
    }
  }, [files]);

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
        <button className="button mr-2" disabled={appId}>
          Generate
        </button>
        <button
          type="button"
          disabled={appId}
          onClick={() => fillInExampleAppDetails()}
          className="button gray"
        >
          Fill in with example app details
        </button>
      </form>

      {files && Object.keys(files).length > 0 && (
        <>
          <header
            className="
         mt-8
         mb-2
         flex
         justify-between
         items-center
          "
          >
            <div
              className="
            flex
            items-center
            
          "
            >
              <h2
                className="
          text-xl
          font-bold
          text-gray-800
          mr-2
        "
              >
                {appName}
              </h2>
              {appId && !generationDone && <Loader />}
            </div>
            <div>
              <button className="button" disabled={!generationDone}>Download the app</button>
            </div>
          </header>
          <div className="grid gap-4 grid-cols-[300px_minmax(900px,_1fr)_100px]">
            <aside>
              <FileTree
                paths={interestingFilePaths}
                activeFilePath={activeFilePath}
                onActivePathSelect={setActiveFilePath}
              />
            </aside>

            {activeFilePath && (
              <main className="flex flex-col gap-2">
                <div className="font-bold">{activeFilePath}:</div>
                <div key={activeFilePath} className="py-4 bg-slate-100 rounded">
                  <CodeHighlight language={language}>
                    {files[activeFilePath].trim()}
                  </CodeHighlight>
                </div>
              </main>
            )}
            {!activeFilePath && (
              <main className="p-8 bg-slate-100 rounded grid place-content-center">
                <div className="text-center">
                  <div className="font-bold">Select a file to view</div>
                  <div className="text-gray-500 text-sm">
                    (click on a file in the file tree)
                  </div>
                </div>
              </main>
            )}
          </div>

          {logs && logs.length > 0 && (
            <>
              {/* <h2
                className="
          text-xl
          font-bold
          text-gray-800
          mt-8
          mb-4
        "
              >
                Logs
              </h2> */}
              <div className="flex flex-col gap-1 mt-8">
                {logs.map((log, i) => (
                  <pre key={i} className="p-3 bg-slate-100 rounded text-sm">
                    {log}
                  </pre>
                ))}
              </div>
            </>
          )}
        </>
      )}
    </div>
  );
};
export default MainPage;
