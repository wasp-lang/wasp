import magicLogo from "../magic-app-gen-logo.png";
import { Link } from "react-router-dom";

export function Title() {
  return (
    <div>
      <div className="flex justify-flex-start items-center">
        <Link to="/">
          <img src={magicLogo} alt="wasp" className="w-20" />
        </Link>
        <div className="text-xl md:text-2xl font-bold text-slate-800 ml-4">
          <h1>MAGE ✨ GPT Web App Generator</h1>
          <p className="md:text-base text-sm leading-relaxed text-gray-500">
            Generate your full-stack web app in Wasp, React, Node.js and Prisma
          </p>
        </div>
      </div>
    </div>
  );
}
