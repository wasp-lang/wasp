import { useState, useEffect, useMemo } from "react";
import getAppGenerationResult from "@wasp/queries/getAppGenerationResult";
import startGeneratingNewApp from "@wasp/actions/startGeneratingNewApp";
import registerZipDownload from "@wasp/actions/registerZipDownload";
import createFeedback from "@wasp/actions/createFeedback";
import { useQuery } from "@wasp/queries";
import { CodeHighlight } from "../components/CodeHighlight";
import { FileTree } from "../components/FileTree";
import { createFilesAndDownloadZip } from "../zip/zipHelpers";
import { useParams } from "react-router-dom";
import { Link } from "react-router-dom";
import { useHistory } from "react-router-dom";
import { RadioGroup } from "@headlessui/react";
import { Loader } from "../components/Loader";
import { MyDialog } from "../components/Dialog";
import { Logs } from "../components/Logs";
import { WaitingRoomContent } from "../components/WaitingRoomContent";
import { Header } from "../components/Header";
import { Faq } from "../components/Faq";
import {
  PiCopyDuotone,
  PiLaptopDuotone,
  PiDownloadDuotone,
  PiCheckDuotone,
  PiGithubLogoDuotone,
  PiStarDuotone,
} from "react-icons/pi";
import { RxQuestionMarkCircled } from "react-icons/rx";
import JSConfetti from "js-confetti";
import getNumProjects from "@wasp/queries/getNumProjects";
import { StatusPill } from "../components/StatusPill";
import { HomeButton, ProfileButton, FaqButton } from "../components/Header";

const jsConfetti = new JSConfetti();

export const ResultPage = () => {
  const { appId } = useParams();
  const [generationDone, setGenerationDone] = useState(false);
  const [isRunning, setIsRunning] = useState(false);
  const [isSuccessModalOpen, setIsSuccessModalOpen] = useState(false);
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
    setCurrentStatus(getStatusPillData(appGenerationResult));
  }, [appGenerationResult, isError]);

  useEffect(() => {
    setGenerationDone(false);
  }, [appId]);

  useEffect(() => {
    const wasRunning = isRunning;
    const currentAppStatus = appGenerationResult?.project?.status;

    setIsRunning(currentAppStatus === "in-progress");

    if (wasRunning && currentAppStatus === "success") {
      setIsSuccessModalOpen(true);
    }
  }, [appGenerationResult?.project?.status]);

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

    const updatedFilePaths = Object.entries(files).reduce((updatedPaths, [path, newContent]) => {
      if (newContent === previousFiles[path]) {
        return updatedPaths;
      }
      return [...updatedPaths, path];
    }, []);

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

  async function downloadZip() {
    const zipName = getUniqueZipName();
    createFilesAndDownloadZip(files, zipName);
    registerZipDownload({
      appId: appGenerationResult?.project?.id,
    });
  }

  function getUniqueZipName() {
    const safeAppName = appGenerationResult?.project?.name.replace(/[^a-zA-Z0-9]/g, "_");
    const randomSuffix = Math.random().toString(36).substring(2, 7);
    return `${safeAppName}-${randomSuffix}`;
  }

  async function retry() {
    const project = appGenerationResult?.project;
    if (
      !project ||
      !project.name ||
      !project.description ||
      !project.primaryColor ||
      !project.authMethod ||
      !project.creativityLevel
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
        appCreativityLevel: project.creativityLevel,
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
        StatusPill={!!appGenerationResult?.project && StatusPill}
      >
        <FaqButton />
        <HomeButton />
        <ProfileButton />
      </Header>
      <OnSuccessModal
        isOpen={isSuccessModalOpen}
        setIsOpen={setIsSuccessModalOpen}
        appGenerationResult={appGenerationResult}
      />

      {isError && (
        <div className="mb-4 bg-red-50 p-8 rounded-xl">
          <div className="text-red-500">
            We couldn't find the app generation result. Maybe the link is incorrect or the app
            generation has failed.
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

      {appGenerationResult?.project.status.includes("deleted") ? (
        <div className="flex flex-col items-center justify-center gap-1 mb-4 bg-red-50 text-gray-700 p-8 rounded-xl">
          <span>This app has been deleted. </span>
          <Link className="underline sm inline-block" to="/">
            &#x2190; Go back and generate a new app
          </Link>
        </div>
      ) : (
        <Logs logs={logs} status={currentStatus.status} onRetry={retry} />
      )}

      <div
        className="overflow-hidden
          flex-row
          space-x-3
          "
      >
        <div
          className={`
            mx-auto flex items-center justify-center divide-white p-3
            text-sm font-medium
            lg:container lg:divide-x lg:px-16 xl:px-20
          `}
        >
          <span
            className="item-center flex gap-2 p-1 px-2 cursor-pointer text-pink-800 bg-pink-200 rounded"
            onClick={() => window.open("https://github.com/wasp-lang/wasp")}
          >
            <span>
              🔮 This is a Wasp powered project. If you like it,{" "}
              <span className="underline">star us on GitHub</span>!
            </span>
          </span>
        </div>
      </div>

      {currentStatus.status === "pending" && (
        <WaitingRoomContent
          numberOfProjectsAheadInQueue={appGenerationResult?.numberOfProjectsAheadInQueue || 0}
        />
      )}

      {interestingFilePaths.length > 0 && (
        <>
          <div className="mb-2 flex items-center justify-between">
            <h2 className="text-xl font-bold text-gray-800">
              {appGenerationResult?.project?.name}
            </h2>
          </div>
          <button
            className="button gray block w-full mb-4 md:hidden"
            onClick={toggleMobileFileBrowser}
          >
            {isMobileFileBrowserOpen ? "Close" : "Open"} file browser ({interestingFilePaths.length}{" "}
            files)
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
              <div>
                <ShareButton />
              </div>

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
              <main className={isMobileFileBrowserOpen ? "hidden md:block" : ""}>
                <div
                  className={`
                    font-bold text-sm bg-slate-200 text-slate-700 p-3 rounded rounded-b-none
                    flex items-center md:justify-between
                  `}
                >
                  <span className="mr-3">{activeFilePath}:</span>
                  <Feedback projectId={appId} />
                </div>
                <div key={activeFilePath} className="py-4 bg-slate-100 rounded rounded-t-none">
                  <CodeHighlight language={language} className="text-sm md:text-base">
                    {files[activeFilePath].trim()}
                  </CodeHighlight>
                </div>
              </main>
            )}
            {!activeFilePath && (
              <main className="p-8 bg-slate-100 rounded grid place-content-center">
                <div className="text-center">
                  <div className="font-bold">Select a file to view</div>
                  <div className="text-gray-500 text-sm">(click on a file in the file tree)</div>
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
      <div className="mt-8">
        <Faq />
      </div>
    </div>
  );
};

function getStatusPillData(generationResult) {
  const backendStatusToPillStatus = {
    pending: "pending",
    "in-progress": "inProgress",
    success: "success",
    failure: "error",
    cancelled: "cancelled",
    deleted: "deleted"
  };

  const queueCardinalNumber = getCardinalNumber(generationResult.numberOfProjectsAheadInQueue);

  const backendStatusToPillText = {
    pending: `${queueCardinalNumber} in the queue`,
    "in-progress": "Generating app",
    success: "Finished",
    failure: "There was an error",
    cancelled: "The generation was cancelled",
    deleted: "The project was deleted"
  };

  return {
    status: backendStatusToPillStatus[generationResult.project.status],
    message: backendStatusToPillText[generationResult.project.status],
  };
}

function getCardinalNumber(number) {
  const lastDigit = number % 10;
  if (lastDigit === 1) {
    return `${number}st`;
  } else if (lastDigit === 2) {
    return `${number}nd`;
  } else if (lastDigit === 3) {
    return `${number}rd`;
  } else {
    return `${number}th`;
  }
}

export function OnSuccessModal({ isOpen, setIsOpen, appGenerationResult }) {
  const [numTokensSpent, setNumTokensSpent] = useState(0);
  const { data: numTotalProjects } = useQuery(getNumProjects, {}, { enabled: isOpen });

  useEffect(() => {
    const logText = appGenerationResult?.project?.logs?.find((log) =>
      log.content.includes("tokens usage")
    )?.content;

    if (logText) {
      const regex = /Total\s+tokens\s+usage\s*:\s*~\s*(\d+(?:\.\d+){0,1})\s*k\b/;
      const match = logText.match(regex);

      if (match) {
        const num = parseFloat(match[1]);
        setNumTokensSpent(num * 1000);
      } else {
        console.log("Failed to parse total number of tokens used: no regex match.");
      }
    }
  }, [appGenerationResult]);

  useEffect(() => {
    if (isOpen) {
      jsConfetti.addConfetti({
        emojis: ["🐝"],
        emojiSize: 120,
      });
    }
  }, [isOpen]);

  function FormattedText({ children }) {
    return <span className="py-1 px-2 font-semibold text-pink-800 rounded">{children}</span>;
  }

  return (
    <MyDialog
      isOpen={isOpen}
      onClose={() => setIsOpen(false)}
      title={<span>Your App is Ready! 🎉</span>}
    >
      <div className="mt-6 space-y-5">
        <p className="text-base leading-relaxed text-gray-500">
          We've made this tool completely <span className="font-semibold">free</span> and cover all
          the costs 😇
        </p>
        {numTokensSpent > 0 && (
          <table className="bg-slate-50 rounded-lg divide-y divide-gray-100 w-full text-base leading-relaxed text-gray-500 text-sm">
            <tbody>
              <tr>
                <td className="p-2 text-gray-600"> Number of tokens your app used: </td>
                <td className="p-2 text-gray-600">
                  {" "}
                  <FormattedText>{numTokensSpent.toLocaleString()}</FormattedText>{" "}
                </td>
              </tr>
              <tr>
                <td className="p-2 text-gray-600"> Cost to generate your app: </td>
                <td className="p-2 text-gray-600">
                  {" "}
                  <FormattedText>{`$${((Number(numTokensSpent) / 1000) * 0.004).toFixed(
                    2
                  )}`}</FormattedText>{" "}
                </td>
              </tr>
              {numTotalProjects && (
                <tr className="p-2 text-gray-600">
                  <td className="p-2 text-gray-600"> Total number of apps generated with Mage: </td>
                  <td className="p-2 text-gray-600">
                    {" "}
                    <FormattedText>{numTotalProjects.toLocaleString()}</FormattedText>{" "}
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        )}
        <p className="text-base leading-relaxed text-gray-500">
          But you can still show your support by starring us on GitHub:
        </p>
        <a
          href="https://github.com/wasp-lang/wasp"
          target="_blank"
          className="flex items-center justify-center underline text-pink-600 "
        >
          <div className="py-4 px-2 flex items-center justify-center bg-pink-50 text-pink-800 rounded-lg font-semibold tracking-wide w-full">
            <PiStarDuotone size="1.35rem" className="mr-3" /> Star Wasp on GitHub{" "}
            <PiGithubLogoDuotone size="1.35rem" className="ml-3" />
          </div>
        </a>
        <p className="text-base leading-relaxed text-gray-500">
          This helps spread the word, so we can keep making Mage better.
        </p>
        <p className="text-base leading-relaxed text-gray-500">We'd very much appreciate it! 🧙</p>
      </div>
    </MyDialog>
  );
}

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
        Run the app locally <PiLaptopDuotone className="inline-block" size={20} />
      </button>
      <MyDialog
        isOpen={showModal}
        onClose={() => setShowModal(false)}
        title={
          <span>
            Run the app locally <PiLaptopDuotone className="inline-block" size={20} />
          </span>
        }
      >
        <div className="mt-6 space-y-6">
          <p className="text-base leading-relaxed text-gray-500">
            Congrats, your full-stack web app is ready! 🎉
            <br />
            App is implemented in{" "}
            <a
              href="https://wasp-lang.dev"
              target="_blank"
              rel="noopener noreferrer"
              className="underline"
            >
              Wasp
            </a>{" "}
            web framework, using React, Node.js and Prisma, and is completely full-stack (frontend +
            backend + database).
          </p>

          <WarningAboutAI />

          <p className="text-base leading-relaxed text-gray-500">Now, let's get the app running!</p>

          <div className="mt-6 bg-slate-100 rounded-lg p-4 text-base text-slate-800">
            <h2 className="font-bold flex items-center space-x-1">
              <span>1. Install Wasp CLI</span>
              <a
                href="https://wasp-lang.dev/docs/quick-start#installation-1"
                target="blank"
                rel="noopener noreferrer"
              >
                {" "}
                <RxQuestionMarkCircled className="text-base" />{" "}
              </a>
              :
            </h2>
            <pre className="mt-2 bg-slate-800 p-4 rounded-lg text-sm text-slate-200">
              curl -sSL https://get.wasp-lang.dev/installer.sh | sh
            </pre>

            <h2 className="font-bold mt-4">
              {" "}
              2. Download the generated app files and unzip them:{" "}
            </h2>
            <button
              className="button flex items-center justify-center gap-1 w-full mt-2"
              onClick={onDownloadZip}
            >
              Download ZIP <PiDownloadDuotone className="inline-block" size={20} />
            </button>

            <h2 className="font-bold mt-4"> 3. Position into the unzipped dir and run the app: </h2>
            <pre className="mt-2 bg-slate-800 p-4 rounded-lg text-sm text-slate-200">
              cd {"<your-app-name>"}
              <br />
              wasp db migrate-dev
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
              Since this is a GPT generated app, it might contain some mistakes, proportional to how
              complex the app is. If there are some in your app, check out{" "}
              <a
                href="https://wasp-lang.dev/docs"
                target="_blank"
                rel="noopener noreferrer"
                className="font-medium text-yellow-600 hover:text-yellow-500 transition ease-in-out duration-150 underline"
              >
                Wasp docs
              </a>{" "}
              for help while fixing them, and also feel free to reach out to us on{" "}
              <a
                href="https://discord.gg/rzdnErX"
                target="_blank"
                rel="noopener noreferrer"
                className="font-medium text-yellow-600 hover:text-yellow-500 transition ease-in-out duration-150 underline"
              >
                Discord
              </a>
              ! You can also try generating the app again to get different results (try playing with
              the creativity level).
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}

function Feedback({ projectId }) {
  const [isModalOpened, setIsModalOpened] = useState(false);
  const [feedbackText, setFeedbackText] = useState("");
  const [score, setScore] = useState(0);

  const scoreOptions = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

  const handleSubmit = async (e) => {
    e.preventDefault();

    try {
      await createFeedback({ score, message: feedbackText, projectId });
    } catch (e) {
      console.error("Could not create feedback");
    }

    setIsModalOpened(false);
    setScore(0);
    setFeedbackText("");
  };

  return (
    <div>
      <button
        className={`
          text-gray-500          
          border border-gray-500
          py-1 px-2 rounded-md mb-1
          flex items-center space-x-2 justify-center
          font-bold
          transition duration-150
          hover:bg-gray-300
        `}
        onClick={() => setIsModalOpened(true)}
      >
        <span>💬 Give us feedback!</span>
      </button>
      <MyDialog
        isOpen={isModalOpened}
        onClose={() => setIsModalOpened(false)}
        title={<span>Let us know how it went!</span>}
      >
        <form onSubmit={handleSubmit}>
          <label className="text-slate-700 block mb-2 mt-8">
            How likely are you to recommend this tool to a friend?{" "}
            <span className="text-red-500">*</span>
          </label>
          <div className="mx-auto w-full max-w-md">
            <RadioGroup value={score} onChange={setScore}>
              <div className="flex space-x-2">
                {scoreOptions.map((option) => (
                  <RadioGroup.Option key={option} value={option}>
                    {({ active, checked }) => (
                      <div
                        className={`
                              ${
                                active
                                  ? "ring-2 ring-white ring-opacity-60 ring-offset-2 ring-offset-sky-300"
                                  : ""
                              }


                              ${checked ? "bg-sky-900 bg-opacity-75 text-white" : ""}
                              cursor-pointer px-3 py-2 shadow-md focus:outline-none
                              rounded-md
                            `}
                      >
                        {option}
                      </div>
                    )}
                  </RadioGroup.Option>
                ))}
              </div>
            </RadioGroup>
          </div>

          <label htmlFor="feedbackText" className="text-slate-700 block mb-2 mt-8">
            How did it go? <span className="text-red-500">*</span>
          </label>
          <textarea
            id="feedback"
            required
            placeholder="How happy are you with the result? What could have been better?"
            value={feedbackText}
            rows="5"
            cols="50"
            onChange={(e) => setFeedbackText(e.target.value)}
          />

          <button className="button black mt-4" type="submit">
            Submit
          </button>
        </form>
      </MyDialog>
    </div>
  );
}

function ShareButton() {
  const [copying, setCopying] = useState(false);
  function copy() {
    navigator.clipboard.writeText(window.location.href);
    setCopying(true);
    setTimeout(() => setCopying(false), 1500);
  }

  return (
    <button
      className="button light-blue flex items-center justify-center gap-1 w-full mb-2"
      onClick={copy}
      disabled={copying}
    >
      {copying ? (
        <span>
          Copied <PiCheckDuotone className="inline-block" size={20} />
        </span>
      ) : (
        <span>
          Copy a shareable link <PiCopyDuotone className="inline-block" size={20} />
        </span>
      )}
    </button>
  );
}
