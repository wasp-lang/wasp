import { useEffect, useMemo, useState } from "react";
import {
  PiGithubLogoDuotone,
  PiMagicWandDuotone,
  PiStarDuotone,
} from "react-icons/pi";
import { useNavigate } from "react-router-dom";

import { GitHubSignInButton, useAuth } from "wasp/client/auth";
import {
  getProjectsByUser,
  startGeneratingNewApp,
  useQuery,
} from "wasp/client/operations";

import { validProjectBrandColors } from "../components/Color";
import { MyDialog } from "../components/Dialog";
import { MyDropdown } from "../components/Dropdown";
import { ExampleCard } from "../components/ExampleCard";
import { Faq } from "../components/Faq";
import { FaqButton, Header, ProfileButton } from "../components/Header";
import { exampleIdeas } from "../examples";
import { readReferrerFromLocalStorage } from "../storage";

const MainPage = () => {
  const [appName, setAppName] = useState("");
  const [appDesc, setAppDesc] = useState("");
  const [appPrimaryColor, setAppPrimaryColor] = useState(
    validProjectBrandColors.find((color) => color.name === "sky"),
  );

  const [isAskForStarsModalOpen, setIsAskForStarsModalOpen] = useState(false);
  const [isLoginModalOpen, setIsLoginModalOpen] = useState(false);
  const [currentStatus, setCurrentStatus] = useState({
    status: "idle",
    message: "Waiting for instructions",
  });
  const navigate = useNavigate();
  const { data: user } = useAuth();
  const { data: userProjects } = useQuery(
    getProjectsByUser,
    {},
    { enabled: !!user },
  );

  const availableCreativityLevels = useMemo(
    () => [
      {
        value: "conventional",
        name: "Conventional",
        description: "Generates sensible code with minimal amount of mistakes.",
        disabled: false,
      },
      {
        value: "balanced",
        name: "Balanced",
        description:
          "Optimal trade-off between creativity and possible mistakes.",
        disabled: false,
      },
      {
        value: "creative",
        name: "Creative",
        description:
          "Generates more creative code, but mistakes are more likely.",
        disabled: false,
      },
    ],
    [],
  );
  const [creativityLevel, setCreativityLevel] = useState(
    availableCreativityLevels.find((lvl) => lvl.value === "balanced"),
  );
  const availableAuthMethods = useMemo(
    () => [
      {
        value: "usernameAndPassword",
        name: "Username & Password",
        disabled: false,
      },
      {
        value: "email",
        name: "Email & Password",
        disabled: true,
      },
      {
        value: "socialAuth",
        name: "Social Auth",
        disabled: true,
      },
    ],
    [],
  );
  const [appAuthMethod, setAppAuthMethod] = useState(availableAuthMethods[0]);

  useEffect(() => {
    if (userProjects?.length === 2) {
      setIsAskForStarsModalOpen(true);
    }
  }, [userProjects]);

  useEffect(() => {
    try {
      const appDetails = JSON.parse(localStorage.getItem("appDetails"));
      if (appDetails) {
        setAppName(appDetails.appName);
        setAppDesc(appDetails.appDesc);
        setAppPrimaryColor(
          validProjectBrandColors.find(
            (color) => color.name === appDetails.appPrimaryColor,
          ),
        );
        setAppAuthMethod(
          availableAuthMethods.find(
            (method) => method.value === appDetails.appAuthMethod,
          ),
        );
        setCreativityLevel(
          availableCreativityLevels.find(
            (level) => level.value === appDetails.appCreativityLevel,
          ),
        );
        localStorage.removeItem("appDetails");
      }
    } catch (error) {
      console.error(error);
    }
  }, []);

  async function startGenerating(event) {
    event.preventDefault();
    try {
      localStorage.setItem(
        "appDetails",
        JSON.stringify({
          appName,
          appDesc,
          appPrimaryColor: appPrimaryColor.name,
          appAuthMethod: appAuthMethod.value,
          appCreativityLevel: creativityLevel.value,
        }),
      );
    } catch (error) {
      console.error(error);
    }

    if (!user) {
      setIsLoginModalOpen(true);
      return;
    }
    setCurrentStatus({
      status: "idle",
      message: "Starting...",
    });

    try {
      const referrer = readReferrerFromLocalStorage();
      const appId = await startGeneratingNewApp({
        referrer,
        appName,
        appDesc,
        appPrimaryColor: appPrimaryColor.name,
        appAuthMethod: appAuthMethod.value,
        appCreativityLevel: creativityLevel.value,
      });
      navigate(`/result/${appId}`);
    } catch (e) {
      setCurrentStatus({
        status: "error",
        message: e.message,
      });
    }
  }

  function useIdea(idea) {
    setAppName(idea.name);
    setAppDesc(idea.description);
    setAppPrimaryColor(idea.color);
    window.scrollTo(0, 0);
  }

  function setAppNameIfValid(value) {
    if (/^[a-zA-Z0-9_-]*$/.test(value)) {
      setAppName(value);
    }
  }

  return (
    <div className="container">
      <Header currentStatus={currentStatus}>
        <FaqButton />
        <ProfileButton setIsLoginModalOpen={setIsLoginModalOpen} />
      </Header>

      <LoginModal isOpen={isLoginModalOpen} setIsOpen={setIsLoginModalOpen} />
      <AskForStarsModal
        isOpen={isAskForStarsModalOpen}
        setIsOpen={setIsAskForStarsModalOpen}
      />

      <form onSubmit={startGenerating} className="rounded-xl bg-slate-50 p-8">
        <div className="mb-6 flex flex-col gap-3">
          <div>
            <label htmlFor="appName" className="mb-2 block text-slate-700">
              App name <span className="text-red-500">*</span>
            </label>
            <input
              id="appName"
              required
              type="text"
              placeholder="e.g. TodoApp or MyPlants"
              value={appName}
              onChange={(e) => setAppNameIfValid(e.target.value)}
              disabled={currentStatus.status === "inProgress"}
            />
          </div>
          <div>
            <label htmlFor="appDesc" className="mb-2 block text-slate-700">
              Description <span className="text-red-500">*</span>
            </label>
            <textarea
              id="appDesc"
              required
              placeholder="Describe your web app in a couple of sentences!
Mention pages you want + what should be happening on them, what data should be stored in the database, etc (check out the examples below).
The simpler and more specific the app is, the better the generated app will be."
              value={appDesc}
              rows="5"
              cols="50"
              onChange={(e) => setAppDesc(e.target.value)}
              disabled={currentStatus.status === "inProgress"}
            />
          </div>
          <div className="grid gap-3 md:grid-cols-3">
            <div>
              <label
                htmlFor="appPrimaryColor"
                className="mb-2 block text-slate-700"
              >
                App brand color
              </label>
              <MyDropdown
                value={appPrimaryColor}
                onChange={setAppPrimaryColor}
                options={validProjectBrandColors}
              />
            </div>
            <div>
              <label
                htmlFor="creativityLevel"
                className="mb-2 block text-slate-700"
              >
                Creativity level
              </label>
              <MyDropdown
                value={creativityLevel}
                onChange={setCreativityLevel}
                options={availableCreativityLevels}
              />
            </div>
            <div>
              <label
                htmlFor="appAuthMethod"
                className="mb-2 block text-slate-700"
              >
                Auth method
              </label>
              <MyDropdown
                value={appAuthMethod}
                onChange={setAppAuthMethod}
                options={availableAuthMethods}
              />
            </div>
          </div>
        </div>
        <button
          className="button mr-2"
          disabled={currentStatus.status === "inProgress"}
        >
          Generate the app <PiMagicWandDuotone className="ml-1 inline-block" />
        </button>
      </form>
      <div className="mt-8">
        <h3 className="mb-4 text-xl font-semibold text-slate-800">
          Some example ideas
        </h3>
        <div className="grid grid-cols-1 gap-2 lg:grid-cols-3 lg:gap-4">
          {exampleIdeas.map((idea) => (
            <ExampleCard key={idea.name} idea={idea} useIdea={useIdea} />
          ))}
        </div>
        <div className="mt-8">
          <Faq />
        </div>
      </div>
    </div>
  );
};
export default MainPage;

export function AskForStarsModal({ isOpen, setIsOpen }) {
  return (
    <MyDialog
      isOpen={isOpen}
      onClose={() => setIsOpen(false)}
      title={<span>With Great Power Comes Great Responsibility! 🧙</span>}
    >
      <div className="mt-6 space-y-2">
        <p className="text-base leading-relaxed text-gray-500">
          Mage is completely <span className="font-semibold">free</span> and we
          cover all the costs.
        </p>
        <p className="text-base leading-relaxed text-gray-500">
          But if you enjoy using it, please consider starring the project on
          GitHub:
        </p>
        <a
          href="https://github.com/wasp-lang/wasp"
          target="_blank"
          className="flex items-center justify-center text-pink-600 underline"
        >
          <div className="mt-4 flex w-full items-center justify-center rounded-lg bg-pink-50 px-2 py-4 font-semibold tracking-wide text-pink-800">
            <PiStarDuotone size="1.35rem" className="mr-3" /> Star Wasp on
            GitHub <PiGithubLogoDuotone size="1.35rem" className="ml-3" />
          </div>
        </a>
      </div>
    </MyDialog>
  );
}

export function LoginModal({ isOpen, setIsOpen }) {
  return (
    <MyDialog
      isOpen={isOpen}
      onClose={() => setIsOpen(false)}
      title={<span>Sign in to your GitHub account</span>}
    >
      <div className="mt-6 space-y-5">
        <p className="text-center text-base leading-relaxed text-gray-500">
          This tool is completely <span className="font-semibold">free</span>.
          <br /> Just sign in with your GitHub Account.
        </p>
        <GitHubSignInButton />
      </div>
    </MyDialog>
  );
}
