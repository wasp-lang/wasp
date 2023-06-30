import { useEffect } from "react";
import Prism from "prismjs";
import "prismjs/components/prism-json";
import addWaspLangauge from "./prism/wasp";
import addPrismaLanguage from "./prism/prisma";
import { DiGithubBadge } from "react-icons/di";

import "./Main.css";
// import { MyDialog } from "./components/Dialog";

addPrismaLanguage(Prism);
addWaspLangauge(Prism);

export function RootComponent({ children }) {
  // const { isAlreadyShown } = useWelcomeDialog();
  // const [isDialogOpen, setIsDialogOpen] = useState(!isAlreadyShown);
  useEffect(() => {
    const script = document.createElement("script");

    script.src = "https://buttons.github.io/buttons.js"; // <----- add your script url
    script.async = true;

    document.body.appendChild(script);

    return () => {
      document.body.removeChild(script);
    };
  }, []);
  return (
    <>
      <div
        className="overflow-hidden
      cursor-pointer flex-row
      space-x-3
      text-white bg-gradient-to-r from-pink-400 to-amber-400"
        onClick={() => window.open("https://github.com/wasp-lang/wasp")}
      >
        <div
          className={`
          mx-auto flex items-center justify-center divide-white p-3
          text-sm font-medium
          lg:container lg:divide-x lg:px-16 xl:px-20
        `}
        >
          <span className="item-center flex gap-2 px-3">
            <span>
              🔮 This is a Wasp powered project. If you like it,{" "}
              <span className="underline">star us on GitHub</span>!
            </span>
          </span>

          <span
            className="space-x-2 px-3"
            style={{
              marginBottom: "-5px",
            }}
          >
            {/* <span
            className={`
              bg-neutral-700 px-2.5 py-1 text-xs rounded-full cursor-pointer
              hover:bg-neutral-600
              flex items-center space-x-1
            `}
          >
            Open Github <DiGithubBadge size={16} className="ml-1"/>
          </span> */}
            <a
              className="github-button"
              href="https://github.com/wasp-lang/wasp"
              data-icon="octicon-star"
              data-show-count="true"
              aria-label="Star wasp-lang/wasp on GitHub"
            >
              Star
            </a>
          </span>
        </div>
      </div>
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {children}
        {/* <MyDialog
        isOpen={isDialogOpen}
        onClose={() => {
          setIsDialogOpen(false);
        }}
        title="Welcome to GPT Web App Generator 👋"
      >
        <p className="leading-relaxed text-gray-500"> 
          This is an experiment by the Wasp team with OpenAI's GPT-4 model. Enjoy and let us know what you think!
        </p>
      </MyDialog> */}
        <footer className="text-center text-gray-500 text-sm mt-8">
          This is an experiment by{" "}
          <a
            href="https://wasp-lang.dev/"
            target="_blank"
            rel="noopener noreferrer"
            className="text-sky-500 hover:text-sky-600"
          >
            Wasp team
          </a>
        </footer>
      </div>
    </>
  );
}

// function useWelcomeDialog() {
//   const isAlreadyShown =
//     window.localStorage.getItem("isAlreadyShown") === "true";
//   useEffect(() => {
//     window.localStorage.setItem("isAlreadyShown", "true");
//   }, []);
//   return { isAlreadyShown };
// }
