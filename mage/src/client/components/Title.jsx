import { Link } from "react-router-dom";
import magicLogo from "../magic-app-gen-logo.png";

export function Title() {
  return (
    <div>
      <div className="justify-flex-start flex items-center">
        <Link to="/">
          <img src={magicLogo} alt="wasp" className="w-20" />
        </Link>
        <div className="ml-4 text-xl font-bold text-slate-800 md:text-2xl">
          <h1>MAGE ✨ GPT Web App Generator</h1>
          <p className="text-sm leading-relaxed text-gray-500 md:text-base">
            Generate your full-stack web app in Wasp, React, Node.js and Prisma
          </p>
        </div>
      </div>
    </div>
  );
}
