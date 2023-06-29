import { useEffect, useState } from "react";
import Prism from "prismjs";
import "prismjs/components/prism-json";
import addWaspLangauge from "./prism/wasp";
import addPrismaLanguage from "./prism/prisma";

import "./Main.css";
import { MyDialog } from "./components/Dialog";

addPrismaLanguage(Prism);
addWaspLangauge(Prism);

export function RootComponent({ children }) {
  const { isAlreadyShown } = useWelcomeDialog();
  const [isDialogOpen, setIsDialogOpen] = useState(!isAlreadyShown);
  return (
    <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
      {children}
      <MyDialog
        isOpen={isDialogOpen}
        onClose={() => {
          setIsDialogOpen(false);
        }}
        title="Welcome to Wasp App Generator 👋"
      >
        <p className="leading-relaxed text-gray-500"> 
          With this tool you can generate a full-stack web app with a single
          command. It will generate a full-stack web app with a database, API
          and a frontend. You can then use the generated code as a starting
          point for your own app.
        </p>
        <p className="leading-relaxed text-gray-500 mt-4">
          Lorem ipsum dolor sit amet consectetur adipisicing elit. Blanditiis
          dignissimos exercitationem atque dicta aspernatur, quis id reiciendis
          facere sequi accusamus possimus hic, dolorum totam eos. Deserunt earum
          culpa nostrum tempore hic praesentium, excepturi cumque vero,
          molestiae quaerat aliquid provident? Corrupti earum hic eos
          repellendus excepturi maxime quia eius doloribus saepe.
        </p>
      </MyDialog>
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
  );
}

function useWelcomeDialog() {
  const isAlreadyShown =
    window.localStorage.getItem("isAlreadyShown") === "true";
  useEffect(() => {
    window.localStorage.setItem("isAlreadyShown", "true");
  }, []);
  return { isAlreadyShown };
}
