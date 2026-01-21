import { RadioGroup } from "@headlessui/react";
import JSConfetti from "js-confetti";
import { useEffect, useMemo, useState } from "react";
import {
  PiCheckDuotone,
  PiCopyDuotone,
  PiDownloadDuotone,
  PiGithubLogoDuotone,
  PiLaptopDuotone,
  PiStarDuotone,
} from "react-icons/pi";
import { RxQuestionMarkCircled } from "react-icons/rx";
import { Link, useNavigate, useParams } from "react-router-dom";

import {
  createFeedback,
  getAppGenerationResult,
  getNumProjects,
  registerZipDownload,
  startGeneratingNewApp,
  useQuery,
} from "wasp/client/operations";

import { CodeHighlight } from "../components/CodeHighlight";
import { MyDialog } from "../components/Dialog";
import { Faq } from "../components/Faq";
import { FileTree } from "../components/FileTree";
import {
  FaqButton,
  Header,
  HomeButton,
  ProfileButton,
} from "../components/Header";
import { Loader } from "../components/Loader";
import { Logs } from "../components/Logs";
import { StatusPill } from "../components/StatusPill";
import { WaitingRoomContent } from "../components/WaitingRoomContent";
import { createFilesAndDownloadZip } from "../zip/zipHelpers";

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
    { enabled: !!appId && !generationDone, refetchInterval: 3000 },
  );
  const [activeFilePath, setActiveFilePath] = useState(null);
  const [currentStatus, setCurrentStatus] = useState({
    status: "idle",
    message: "Waiting",
  });
  const [currentFiles, setCurrentFiles] = useState({});
  const navigate = useNavigate();
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

    const updatedFilePaths = Object.entries(files).reduce(
      (updatedPaths, [path, newContent]) => {
        if (newContent === previousFiles[path]) {
          return updatedPaths;
        }
        return [...updatedPaths, path];
      },
      [],
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
            path !== "src/vite-env.d.ts" &&
            path !== "tsconfig.json" &&
            path !== ".gitignore" &&
            path !== ".waspignore" &&
            path !== "public/.gitkeep" &&
            path !== ".wasproot",
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
    const safeAppName = appGenerationResult?.project?.name.replace(
      /[^a-zA-Z0-9]/g,
      "_",
    );
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
      alert("Okay, redirecting to the new attempt");
      navigate(`/result/${appId}`);
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
        <div className="mb-4 rounded-xl bg-red-50 p-8">
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
          <header className="big-box items-flex-start mt-4 mb-4 flex justify-between">
            <div className="mr-3 shrink-0">
              <Loader />
            </div>
            <pre className="flex-1">Fetching the app...</pre>
          </header>
        </>
      )}

      {appGenerationResult?.project.status.includes("deleted") ? (
        <div className="mb-4 flex flex-col items-center justify-center gap-1 rounded-xl bg-red-50 p-8 text-gray-700">
          <span>This app has been deleted. </span>
          <Link className="sm inline-block underline" to="/">
            &#x2190; Go back and generate a new app
          </Link>
        </div>
      ) : (
        <Logs logs={logs} status={currentStatus.status} onRetry={retry} />
      )}

      <div className="flex-row space-x-3 overflow-hidden">
        <div
          className={`mx-auto flex items-center justify-center divide-white p-3 text-sm font-medium lg:container lg:divide-x lg:px-16 xl:px-20`}
        >
          <span
            className="item-center flex cursor-pointer gap-2 rounded-sm bg-pink-200 p-1 px-2 text-pink-800"
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
          numberOfProjectsAheadInQueue={
            appGenerationResult?.numberOfProjectsAheadInQueue || 0
          }
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
            className="button gray mb-4 block w-full md:hidden"
            onClick={toggleMobileFileBrowser}
          >
            {isMobileFileBrowserOpen ? "Close" : "Open"} file browser (
            {interestingFilePaths.length} files)
          </button>
          <div className="mt-4 grid gap-4 overflow-x-auto md:grid-cols-[320px_1fr] md:overflow-x-visible">
            <aside className={isMobileFileBrowserOpen ? "" : "hidden md:block"}>
              <div className="mb-2">
                <RunTheAppModal
                  onDownloadZip={downloadZip}
                  disabled={currentStatus.status !== "success"}
                />
              </div>
              {currentStatus.status !== "success" && (
                <small className="my-2 block text-center text-gray-500">
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
              <p className="my-4 hidden text-sm leading-relaxed text-gray-500 md:block">
                <strong>User provided prompt: </strong>
                {appGenerationResult?.project?.description}
              </p>
            </aside>

            {activeFilePath && (
              <main
                className={isMobileFileBrowserOpen ? "hidden md:block" : ""}
              >
                <div
                  className={`flex items-center rounded-sm rounded-b-none bg-slate-200 p-3 text-sm font-bold text-slate-700 md:justify-between`}
                >
                  <span className="mr-3">{activeFilePath}:</span>
                  <Feedback projectId={appId} />
                </div>
                <div
                  key={activeFilePath}
                  className="rounded-sm rounded-t-none bg-slate-100 py-4"
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
              <main className="grid place-content-center rounded-sm bg-slate-100 p-8">
                <div className="text-center">
                  <div className="font-bold">Select a file to view</div>
                  <div className="text-sm text-gray-500">
                    (click on a file in the file tree)
                  </div>
                </div>
              </main>
            )}
          </div>
          <p className="my-4 text-sm leading-relaxed text-gray-500 md:hidden">
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
    deleted: "deleted",
  };

  const queueCardinalNumber = getCardinalNumber(
    generationResult.numberOfProjectsAheadInQueue,
  );

  const backendStatusToPillText = {
    pending: `${queueCardinalNumber} in the queue`,
    "in-progress": "Generating app",
    success: "Finished",
    failure: "There was an error",
    cancelled: "The generation was cancelled",
    deleted: "The project was deleted",
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
  const { data: numTotalProjects } = useQuery(
    getNumProjects,
    {},
    { enabled: isOpen },
  );

  useEffect(() => {
    const logText = appGenerationResult?.project?.logs?.find((log) =>
      log.content.includes("tokens usage"),
    )?.content;

    if (logText) {
      const regex =
        /Total\s+tokens\s+usage\s*:\s*~\s*(\d+(?:\.\d+){0,1})\s*k\b/;
      const match = logText.match(regex);

      if (match) {
        const num = parseFloat(match[1]);
        setNumTokensSpent(num * 1000);
      } else {
        console.log(
          "Failed to parse total number of tokens used: no regex match.",
        );
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
    return (
      <span className="rounded-sm px-2 py-1 font-semibold text-pink-800">
        {children}
      </span>
    );
  }

  function calcCostForGpt_4o(numTokensSpent) {
    const estimatedInputTokenShare = 0.8;
    const estimatedOutputTokenShare = 1 - estimatedInputTokenShare;
    const costInUsdForMillionInputTokens = 5.0; // This is price for gpt-4o-2024-05-13.
    const costInUsdForMillionOutputTokens = 15.0; // This is price for gpt-4o-2024-05-13.
    const costInUsdForMillionTokens =
      costInUsdForMillionInputTokens * estimatedInputTokenShare +
      costInUsdForMillionOutputTokens * estimatedOutputTokenShare;
    return ((numTokensSpent / 1e6) * costInUsdForMillionTokens).toFixed(2);
  }

  return (
    <MyDialog
      isOpen={isOpen}
      onClose={() => setIsOpen(false)}
      title={<span>Your App is Ready! 🎉</span>}
    >
      <div className="mt-6 space-y-5">
        <p className="text-base leading-relaxed text-gray-500">
          We've made this tool completely{" "}
          <span className="font-semibold">free</span> and cover all the costs 😇
        </p>
        {numTokensSpent > 0 && (
          <table className="w-full divide-y divide-gray-100 rounded-lg bg-slate-50 text-sm leading-relaxed text-gray-500">
            <tbody>
              <tr>
                <td className="p-2 text-gray-600">
                  {" "}
                  Number of tokens your app used:{" "}
                </td>
                <td className="p-2 text-gray-600">
                  {" "}
                  <FormattedText>
                    {numTokensSpent.toLocaleString()}
                  </FormattedText>{" "}
                </td>
              </tr>
              <tr>
                <td className="p-2 text-gray-600">
                  {" "}
                  Cost to generate your app:{" "}
                </td>
                <td className="p-2 text-gray-600">
                  {" "}
                  <FormattedText>{`~$${calcCostForGpt_4o(Number(numTokensSpent))}`}</FormattedText>{" "}
                </td>
              </tr>
              {numTotalProjects && (
                <tr className="p-2 text-gray-600">
                  <td className="p-2 text-gray-600">
                    {" "}
                    Total number of apps generated with Mage:{" "}
                  </td>
                  <td className="p-2 text-gray-600">
                    {" "}
                    <FormattedText>
                      {numTotalProjects.toLocaleString()}
                    </FormattedText>{" "}
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
          className="flex items-center justify-center text-pink-600 underline"
        >
          <div className="flex w-full items-center justify-center rounded-lg bg-pink-50 px-2 py-4 font-semibold tracking-wide text-pink-800">
            <PiStarDuotone size="1.35rem" className="mr-3" /> Star Wasp on
            GitHub <PiGithubLogoDuotone size="1.35rem" className="ml-3" />
          </div>
        </a>
        <p className="text-base leading-relaxed text-gray-500">
          This helps spread the word, so we can keep making Mage better.
        </p>
        <p className="text-base leading-relaxed text-gray-500">
          We'd very much appreciate it! 🧙
        </p>
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
          !disabled ? "animate-jumping" : ""
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
            <br />
            App is implemented in{" "}
            <a
              href="https://wasp.sh"
              target="_blank"
              rel="noopener noreferrer"
              className="underline"
            >
              Wasp
            </a>{" "}
            web framework, using React, Node.js and Prisma, and is completely
            full-stack (frontend + backend + database).
          </p>

          <WarningAboutAI />

          <p className="text-base leading-relaxed text-gray-500">
            Now, let's get the app running!
          </p>

          <div className="mt-6 rounded-lg bg-slate-100 p-4 text-base text-slate-800">
            <h2 className="flex items-center space-x-1 font-bold">
              <span>1. Install Wasp CLI (Linux / Mac / Win+WSL)</span>
              <a
                href="https://wasp.sh/docs/quick-start#installation-1"
                target="blank"
                rel="noopener noreferrer"
              >
                {" "}
                <RxQuestionMarkCircled className="text-base" />{" "}
              </a>
              :
            </h2>
            <pre className="mt-2 rounded-lg bg-slate-800 p-4 text-sm text-slate-200">
              npm i -g @wasp.sh/wasp-cli@latest
            </pre>

            <h2 className="mt-4 font-bold">
              {" "}
              2. Download the generated app files and unzip them:{" "}
            </h2>
            <button
              className="button mt-2 flex w-full items-center justify-center gap-1"
              onClick={onDownloadZip}
            >
              Download ZIP{" "}
              <PiDownloadDuotone className="inline-block" size={20} />
            </button>

            <h2 className="mt-4 font-bold">
              {" "}
              3. Position into the unzipped dir and run the app:{" "}
            </h2>
            <pre className="mt-2 rounded-lg bg-slate-800 p-4 text-sm text-slate-200">
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

          <div className="rounded-sm bg-pink-50 p-4 text-pink-800">
            If you like this project,{" "}
            <a
              href="https://github.com/wasp-lang/wasp"
              target="_blank"
              className="text-pink-600 underline"
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
    <div className="rounded-sm bg-yellow-50 p-4 text-yellow-700">
      <div className="flex">
        <div className="ml-3">
          <p className="text-sm leading-5 font-medium">⚠️ Experimental tech</p>
          <div className="mt-2 text-sm leading-5">
            <p>
              Since this is a GPT generated app, it might contain some mistakes,
              proportional to how complex the app is. If there are some in your
              app, check out{" "}
              <a
                href="https://wasp.sh/docs"
                target="_blank"
                rel="noopener noreferrer"
                className="font-medium text-yellow-600 underline transition duration-150 ease-in-out hover:text-yellow-500"
              >
                Wasp docs
              </a>{" "}
              for help while fixing them, and also feel free to reach out to us
              on{" "}
              <a
                href="https://discord.gg/rzdnErX"
                target="_blank"
                rel="noopener noreferrer"
                className="font-medium text-yellow-600 underline transition duration-150 ease-in-out hover:text-yellow-500"
              >
                Discord
              </a>
              ! You can also try generating the app again to get different
              results (try playing with the creativity level).
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
        className={`mb-1 flex items-center justify-center space-x-2 rounded-md border border-gray-500 px-2 py-1 font-bold text-gray-500 transition duration-150 hover:bg-gray-300`}
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
          <label className="mt-8 mb-2 block text-slate-700">
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
                        className={` ${
                          active
                            ? "ring-2 ring-white/60 ring-offset-2 ring-offset-sky-300"
                            : ""
                        } ${checked ? "bg-opacity-75 bg-sky-900 text-white" : ""} cursor-pointer rounded-md px-3 py-2 shadow-md focus:outline-hidden`}
                      >
                        {option}
                      </div>
                    )}
                  </RadioGroup.Option>
                ))}
              </div>
            </RadioGroup>
          </div>

          <label
            htmlFor="feedbackText"
            className="mt-8 mb-2 block text-slate-700"
          >
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
      className="button light-blue mb-2 flex w-full items-center justify-center gap-1"
      onClick={copy}
      disabled={copying}
    >
      {copying ? (
        <span>
          Copied <PiCheckDuotone className="inline-block" size={20} />
        </span>
      ) : (
        <span>
          Copy a shareable link{" "}
          <PiCopyDuotone className="inline-block" size={20} />
        </span>
      )}
    </button>
  );
}
