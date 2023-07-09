import { useState, useEffect, useMemo } from "react";
import getAppGenerationResult from "@wasp/queries/getAppGenerationResult";
import startGeneratingNewApp from "@wasp/actions/startGeneratingNewApp";
import { useQuery } from "@wasp/queries";
import { CodeHighlight } from "../components/CodeHighlight";
import { FileTree } from "../components/FileTree";
import { createFilesAndDownloadZip } from "../zip/zipHelpers";
import { useParams } from "react-router-dom";
import { Link } from "react-router-dom";
import { useHistory } from "react-router-dom";
import { Loader } from "../components/Loader";
import { MyDialog } from "../components/Dialog";
import { Logs } from "../components/Logs";
import { Header } from "../components/Header";
import {
  PiCopyDuotone,
  PiLaptopDuotone,
  PiDownloadDuotone,
  PiCheckDuotone,
} from "react-icons/pi";

export const ResultPage = () => {
  const { appId } = useParams();
  const [generationDone, setGenerationDone] = useState(false);
  const {
    data: appGenerationResult,
    isError,
    isLoading,
  } = useQuery(
    getAppGenerationResult,
    { appId },
    { enabled: !!appId && !generationDone, refetchInterval: 3000 }
  );
  const [activeFilePath, setActiveFilePath] = useState(null);
  const [currentStatus, setCurrentStatus] = useState({
    status: "idle",
    message: "Waiting",
  });
  const [currentFiles, setCurrentFiles] = useState({});
  const history = useHistory();
  const [isMobileFileBrowserOpen, setIsMobileFileBrowserOpen] = useState(false);

  useEffect(() => {
    const backendStatusToPillStatus = {
      pending: "pending",
      "in-progress": "inProgress",
      success: "success",
      failure: "error",
      cancelled: "cancelled",
    };
    const backendStatusToPillText = {
      pending: "In the queue",
      "in-progress": "Generating app",
      success: "Finished",
      failure: "There was an error",
      cancelled: "The generation was cancelled",
    };
    if (!appGenerationResult?.project) {
      return;
    }
    if (
      appGenerationResult?.project?.status === "success" ||
      appGenerationResult?.project?.status === "failure" ||
      appGenerationResult?.project?.status === "cancelled" ||
      isError
    ) {
      setGenerationDone(true);
    }
    setCurrentStatus({
      status: backendStatusToPillStatus[appGenerationResult.project.status],
      message: backendStatusToPillText[appGenerationResult.project.status],
    });
  }, [appGenerationResult, isError]);

  useEffect(() => {
    setGenerationDone(false);
  }, [appId]);

  const logs = appGenerationResult?.project?.logs.map((log) => log.content);

  const files = useMemo(() => {
    let files = {};
    (appGenerationResult?.project?.files ?? []).reduce((acc, file) => {
      acc[file.name] = file.content;
      return acc;
    }, files);
    return files;
  }, [appGenerationResult]);

  const freshlyUpdatedFilePaths = useMemo(() => {
    const previousFiles = currentFiles;
    setCurrentFiles(files);

    if (Object.keys(previousFiles).length === 0) {
      return [];
    }

    const updatedFilePaths = Object.entries(files).reduce(
      (updatedPaths, [path, newContent]) => {
        if (newContent === previousFiles[path]) {
          return updatedPaths;
        }
        return [...updatedPaths, path];
      },
      []
    );

    return updatedFilePaths;
  }, [files]);

  const language = useMemo(() => {
    if (activeFilePath) {
      const ext = activeFilePath.split(".").pop();
      if (["jsx", "tsx", "js", "ts", "cjs"].includes(ext)) {
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
        .sort((a, b) => {
          if (a.endsWith(".wasp") && !b.endsWith(".wasp")) {
            return -1;
          }
          if (!a.endsWith(".wasp") && b.endsWith(".wasp")) {
            return 1;
          }
          return a.split("/").length - b.split("/").length;
        });
    } else {
      return [];
    }
  }, [files]);

  useEffect(() => {
    if (activeFilePath === null && interestingFilePaths.length > 0) {
      setActiveFilePath(interestingFilePaths[0]);
    }
  }, [interestingFilePaths]);

  useEffect(() => {
    setIsMobileFileBrowserOpen(false);
  }, [activeFilePath]);

  function downloadZip() {
    const safeAppName = appGenerationResult?.project?.name.replace(
      /[^a-zA-Z0-9]/g,
      "_"
    );
    const randomSuffix = Math.random().toString(36).substring(2, 7);
    const appNameWithSuffix = `${safeAppName}-${randomSuffix}`;
    createFilesAndDownloadZip(files, appNameWithSuffix);
  }

  async function retry() {
    const project = appGenerationResult?.project;
    if (
      !project ||
      !project.name ||
      !project.description ||
      !project.primaryColor ||
      !project.authMethod
    ) {
      alert("Missing project data");
      return;
    }
    try {
      const appId = await startGeneratingNewApp({
        appName: project.name,
        appDesc: project.description,
        appPrimaryColor: project.primaryColor,
        appAuthMethod: project.authMethod,
      });
      alert("Now I will go to " + appId);
      history.push(`/result/${appId}`);
    } catch (e) {
      alert(e.message);
    }
  }

  function toggleMobileFileBrowser() {
    setIsMobileFileBrowserOpen((isOpen) => !isOpen);
  }

  return (
    <div className="container">
      <Header
        currentStatus={currentStatus}
        isStatusVisible={!!appGenerationResult?.project}
      />

      {isError && (
        <div className="mb-4 bg-red-50 p-8 rounded-xl">
          <div className="text-red-500">
            We couldn't find the app generation result. Maybe the link is
            incorrect or the app generation has failed.
          </div>
          <Link className="button gray sm mt-4 inline-block" to="/">
            Generate a new one
          </Link>
        </div>
      )}

      {isLoading && (
        <>
          <header className="big-box mt-4 mb-4 flex justify-between items-flex-start">
            <div className="flex-shrink-0 mr-3">
              <Loader />
            </div>
            <pre className="flex-1">Fetching the app...</pre>
          </header>
        </>
      )}

      <Logs logs={logs} status={currentStatus.status} onRetry={retry} />

      {interestingFilePaths.length > 0 && (
        <>
          <div className="mb-2 flex items-center justify-between">
            <h2 className="text-xl font-bold text-gray-800">
              {appGenerationResult?.project?.name}
            </h2>
            <div>
              <CopyLink />
            </div>
          </div>
          <button
            className="button gray block w-full mb-4 md:hidden"
            onClick={toggleMobileFileBrowser}
          >
            {isMobileFileBrowserOpen ? "Close" : "Open"} file browser (
            {interestingFilePaths.length} files)
          </button>
          <div className="grid gap-4 md:grid-cols-[320px_1fr] mt-4 overflow-x-auto md:overflow-x-visible">
            <aside className={isMobileFileBrowserOpen ? "" : "hidden md:block"}>
              <div className="mb-2">
                <RunTheAppModal
                  onDownloadZip={downloadZip}
                  disabled={currentStatus.status !== "success"}
                />
              </div>
              {currentStatus.status !== "success" && (
                <small className="text-gray-500 text-center block my-2">
                  The app is still being generated.
                </small>
              )}
              <FileTree
                paths={interestingFilePaths}
                activeFilePath={activeFilePath}
                onActivePathSelect={setActiveFilePath}
                freshlyUpdatedPaths={freshlyUpdatedFilePaths}
              />
              <p className="text-gray-500 text-sm my-4 leading-relaxed hidden md:block">
                <strong>User provided prompt: </strong>
                {appGenerationResult?.project?.description}
              </p>
            </aside>

            {activeFilePath && (
              <main
                className={isMobileFileBrowserOpen ? "hidden md:block" : ""}
              >
                <div className="font-bold text-sm bg-slate-200 text-slate-700 p-3 rounded rounded-b-none">
                  {activeFilePath}:
                </div>
                <div
                  key={activeFilePath}
                  className="py-4 bg-slate-100 rounded rounded-t-none"
                >
                  <CodeHighlight
                    language={language}
                    className="text-sm md:text-base"
                  >
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
          <p className="text-gray-500 text-sm my-4 leading-relaxed md:hidden">
            <strong>User provided prompt: </strong>
            {appGenerationResult?.project?.description}
          </p>
        </>
      )}
    </div>
  );
};

export default function RunTheAppModal({ disabled, onDownloadZip }) {
  const [showModal, setShowModal] = useState(false);
  return (
    <>
      <button
        className={`button flex items-center justify-center gap-1 w-full${
          !disabled ? " animate-jumping" : ""
        }`}
        disabled={disabled}
        onClick={() => setShowModal(true)}
      >
        Run the app locally{" "}
        <PiLaptopDuotone className="inline-block" size={20} />
      </button>
      <MyDialog
        isOpen={showModal}
        onClose={() => setShowModal(false)}
        title={
          <span>
            Run the app locally{" "}
            <PiLaptopDuotone className="inline-block" size={20} />
          </span>
        }
      >
        <div className="mt-6 space-y-6">
          <p className="text-base leading-relaxed text-gray-500">
            Congrats, your full-stack web app is ready! 🎉
            <br/>
            App is implemented in{" "}
            <a
              href="https://wasp-lang.dev"
              target="_blank"
              rel="noopener noreferrer"
              className="underline"
            >
              Wasp
            </a>{" "}
            web framework, using React, Node.js and Prisma, and is completely full-stack (frontend + backend + database).
          </p>

          <WarningAboutAI />

          <p className="text-base leading-relaxed text-gray-500">
            Now, let's download and run it!
          </p>

          <div className="mt-6 bg-slate-100 rounded-lg p-4 text-base text-slate-800">
            <h2 className="font-bold"> 1. Install Wasp CLI: </h2>
            <pre className="mt-2 bg-slate-800 p-4 rounded-lg text-sm text-slate-200">
              curl -sSL https://get.wasp-lang.dev/installer.sh | sh
            </pre>

            <h2 className="font-bold mt-4"> 2. Download the generated app files and unzip them: </h2>
            <button
              className="button flex items-center justify-center gap-1 w-full mt-2"
              onClick={onDownloadZip}
            >
              Download ZIP{" "}
              <PiDownloadDuotone className="inline-block" size={20} />
            </button>

            <h2 className="font-bold mt-4"> 3. Position into the unzipped dir and run the app: </h2>
            <pre className="mt-2 bg-slate-800 p-4 rounded-lg text-sm text-slate-200">
              cd {"<your-app-name>"}
              <br />
              wasp db migrate-dev  <span className="text-slate-400"># init the db</span>
              <br />
              wasp start
            </pre>
          </div>

          <p className="text-base leading-relaxed text-gray-500">
            Congratulations, you are now running your app! 🎉
          </p>

          <div className="bg-pink-50 text-pink-800 p-4 rounded">
            If you like this project,{" "}
            <a
              href="https://github.com/wasp-lang/wasp"
              target="_blank"
              className="underline text-pink-600"
            >
              star us on GitHub
            </a>{" "}
            ⭐️
          </div>
        </div>
      </MyDialog>
    </>
  );
}

function WarningAboutAI() {
  return (
    <div className="bg-yellow-50 text-yellow-700 p-4 rounded">
      <div className="flex">
        <div className="ml-3">
          <p className="text-sm leading-5 font-medium">⚠️ Experimental tech</p>
          <div className="mt-2 text-sm leading-5">
            <p>
              Since this is a GPT generated app, it might contain some mistakes, proportional to how complex the app is.
              If there are some in your app, check out {" "}
              <a
                href="https://wasp-lang.dev/docs"
                target="_blank"
                rel="noopener noreferrer"
                className="font-medium text-yellow-600 hover:text-yellow-500 transition ease-in-out duration-150 underline"
              >
                Wasp docs
              </a>
              {" "}for help while fixing them, and also feel free to reach out to us on{" "}
              <a
                href="https://discord.gg/rzdnErX"
                target="_blank"
                rel="noopener noreferrer"
                className="font-medium text-yellow-600 hover:text-yellow-500 transition ease-in-out duration-150 underline"
              >
                Discord
              </a>
              !
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}

function CopyLink() {
  const [copying, setCopying] = useState(false);
  function copy() {
    navigator.clipboard.writeText(window.location.href);
    setCopying(true);
    setTimeout(() => setCopying(false), 1500);
  }

  return (
    <button
      className="copy-link flex items-center justify-center gap-1"
      onClick={copy}
      disabled={copying}
    >
      {copying ? (
        <span>
          Copied <PiCheckDuotone className="inline-block" size={20} />
        </span>
      ) : (
        <span>
          Copy a sharable link{" "}
          <PiCopyDuotone className="inline-block" size={20} />
        </span>
      )}
    </button>
  );
}
