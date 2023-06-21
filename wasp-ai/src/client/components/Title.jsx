import waspLogo from "../waspLogo.png";
import { Link } from "react-router-dom";

export function Title() {
  return (
    <div className="flex justify-flex-start items-center">
      <img src={waspLogo} alt="wasp" className="w-16" />
      <h1 className="text-2xl font-bold text-slate-800 ml-4">
        <Link to="/">Wasp App Generator</Link>
        <div className="mt-2 flex justify-flex-start">
          <iframe
            src="https://ghbtns.com/github-btn.html?user=wasp-lang&repo=wasp&type=star&count=true"
            frameborder="0"
            width="100"
            height="20"
            title="GitHub"
          ></iframe>
        </div>
      </h1>
    </div>
  );
}
