import { useState, useEffect, useMemo } from "react";
import getAppGenerationResult from "@wasp/queries/getAppGenerationResult";
import { useQuery } from "@wasp/queries";
import { CodeHighlight } from "../components/CodeHighlight";
import { FileTree } from "../components/FileTree";
import { createFilesAndDownloadZip } from "../zip/zipHelpers";
import { StatusPill } from "../components/StatusPill";
import { useParams } from "react-router-dom";
import { Link } from "react-router-dom";

export const ResultPage = () => {
  const { appId } = useParams();
  const [generationDone, setGenerationDone] = useState(false);
  const { data: appGenerationResult } = useQuery(
    getAppGenerationResult,
    { appId },
    { enabled: !!appId && !generationDone, refetchInterval: 3000 }
  );
  const [activeFilePath, setActiveFilePath] = useState(null);
  const [currentStatus, setCurrentStatus] = useState({
    status: "idle",
    message: "Waiting for instructions",
  });
  const [logsVisible, setLogsVisible] = useState(false);

  useEffect(() => {
    if (
      appGenerationResult?.status === "success" ||
      appGenerationResult?.status === "failure"
    ) {
      setGenerationDone(true);
      setCurrentStatus({
        status: appGenerationResult.status === "success" ? "success" : "error",
        message:
          appGenerationResult.status === "success"
            ? "Finished"
            : "There was an error",
      });
    } else {
      setCurrentStatus({
        status: "inProgress",
        message: "Generating app",
      });
    }
  }, [appGenerationResult]);

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

  function downloadZip() {
    const safeAppName = appGenerationResult.appName.replace(
      /[^a-zA-Z0-9]/g,
      "_"
    );
    createFilesAndDownloadZip(files, safeAppName);
  }

  function toggleLogs() {
    setLogsVisible(!logsVisible);
  }

  return (
    <div className="container">
      <div className="mb-4 bg-slate-50 p-8 rounded-xl flex justify-between items-center">
        <Title />
        <StatusPill status={currentStatus.status}>
          {currentStatus.message}
        </StatusPill>
      </div>

      <header className="mt-4 bg-slate-900 text-white p-8 rounded-xl flex justify-between items-center">
        <div className="flex-shrink-0 mr-3">
          <Loader />
        </div>
        <pre className="flex-1">{logs && logs.length > 0 ? logs[0] : "Waiting for logs..."}</pre>
        <button onClick={toggleLogs}>
          {logsVisible ? "Hide the logs" : "Expand the logs"}
        </button>
      </header>

      {interestingFilePaths.length > 0 && (
        <>
          <div className="grid gap-4 grid-cols-[300px_minmax(900px,_1fr)_100px] mt-4">
            <aside>
              <div className="mb-2">
                <h2 className="text-xl font-bold text-gray-800">
                  {appGenerationResult.appName}
                </h2>
              </div>
              <FileTree
                paths={interestingFilePaths}
                activeFilePath={activeFilePath}
                onActivePathSelect={setActiveFilePath}
              />
              <RunTheAppModal
                onDownloadZip={downloadZip}
                disabled={currentStatus.status !== "success"}
              />
              {currentStatus.status === "success" && (
                <Link className="button gray w-full mt-2 block" to="/">
                  Generate another one?
                </Link>
              )}
            </aside>

            {activeFilePath && (
              <main>
                <div className="font-bold text-sm bg-slate-200 text-slate-700 p-3 rounded rounded-b-none">
                  {activeFilePath}:
                </div>
                <div
                  key={activeFilePath}
                  className="py-4 bg-slate-100 rounded rounded-t-none"
                >
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
        </>
      )}
    </div>
  );
};

import React from "react";
import { Title } from "../components/Title";
import { Loader } from "../components/Loader";

export default function RunTheAppModal({ disabled, onDownloadZip }) {
  const [showModal, setShowModal] = React.useState(false);
  return (
    <>
      <button
        className="button w-full mt-2"
        disabled={disabled}
        onClick={() => setShowModal(true)}
      >
        Run the app locally ⚡️
      </button>
      {showModal ? (
        <>
          <div className="justify-center items-center flex overflow-x-hidden overflow-y-auto fixed inset-0 z-50 outline-none focus:outline-none">
            <div className="relative w-auto my-6 mx-auto max-w-3xl">
              <div className="border-0 rounded-lg shadow-lg relative flex flex-col w-full bg-white outline-none focus:outline-none">
                <div className="flex items-center justify-between p-5 border-b border-solid border-slate-200 rounded-t">
                  <h3 className="text-xl font-semibold text-gray-900">
                    Run the app locally ⚡️
                  </h3>
                  <button
                    type="button"
                    class="text-gray-400 bg-transparent hover:bg-gray-200 hover:text-gray-900 rounded-lg text-sm p-1.5 ml-auto inline-flex items-center"
                    onClick={() => setShowModal(false)}
                  >
                    <svg
                      aria-hidden="true"
                      class="w-5 h-5"
                      fill="currentColor"
                      viewBox="0 0 20 20"
                      xmlns="http://www.w3.org/2000/svg"
                    >
                      <path
                        fill-rule="evenodd"
                        d="M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                        clip-rule="evenodd"
                      ></path>
                    </svg>
                    <span class="sr-only">Close modal</span>
                  </button>
                </div>
                <div className="p-6 space-y-6">
                  <p className="text-base leading-relaxed text-gray-500">
                    First, you need to install Wasp locally. You can do that by
                    running this command in your terminal:
                  </p>
                  <pre className="bg-slate-50 p-4 rounded-lg text-sm">curl -sSL https://get.wasp-lang.dev/installer.sh | sh</pre>
                  <p className="text-base leading-relaxed text-gray-500">
                    Then, you download the ZIP file with the generated app:
                  </p>
                  <button
                    className="button w-full"
                    onClick={onDownloadZip}
                  >
                    Download ZIP
                  </button>
                  <p className="text-base leading-relaxed text-gray-500">
                    Unzip the file and run the app with:
                  </p>
                  <pre className="bg-slate-50 p-4 rounded-lg text-sm">wasp start</pre>
                  <p className="text-base leading-relaxed text-gray-500">
                    Congratulations, you are now running your Wasp app locally! 🎉
                  </p>
                </div>
              </div>
            </div>
          </div>
          <div className="opacity-25 fixed inset-0 z-40 bg-black"></div>
        </>
      ) : null}
    </>
  );
}
