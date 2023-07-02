import magicLogo from "../magic-app-gen-logo.png";
import { Link } from "react-router-dom";

export function Title() {
  return (
    <div>
      <Link to="/" className="flex justify-flex-start items-center">
        <img src={magicLogo} alt="wasp" className="w-20" />
        <h1 className="text-xl md:text-2xl font-bold text-slate-800 ml-4">
          GPT Web App Generator ✨
          <p className="md:text-base text-sm leading-relaxed text-gray-500">
            Generate your full-stack web app in Wasp, React, Node.js and Prisma
          </p>
        </h1>
      </Link>
    </div>
  );
}
