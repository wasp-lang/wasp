import magicLogo from "../magic-app-gen-logo.png";
import { Link } from "react-router-dom";

export function Title() {
  return (
    <div className="flex justify-flex-start items-center">
      <img src={magicLogo} alt="wasp" className="w-20" />
      <h1 className="text-2xl font-bold text-slate-800 ml-4">
        <Link to="/">
          GPT Web App Generator ✨
        </Link>
        <p className="text-base leading-relaxed text-gray-500">
          Generate your full-stack web app in Wasp, React, Node.js and Prisma
        </p>
        {/* <div className="mt-2 flex justify-flex-start">
          <iframe
            src="https://ghbtns.com/github-btn.html?user=wasp-lang&repo=wasp&type=star&count=true"
            width="100"
            height="20"
            title="GitHub"
          ></iframe>
        </div> */}
      </h1>
    </div>
  );
}
