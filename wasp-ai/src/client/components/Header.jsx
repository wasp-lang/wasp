import { StatusPill } from "./StatusPill";
import { Title } from "./Title";
import { signInUrl as gitHubSignInUrl } from "@wasp/auth/helpers/GitHub";
import { AiFillGithub } from "react-icons/ai";
import { BiSolidUser } from "react-icons/bi";
import { RxQuestionMarkCircled } from "react-icons/rx";
import useAuth from "@wasp/auth/useAuth";
import { Link } from "@wasp/router";


export function Header({ currentStatus, isStatusVisible }) {
  const { data: user } = useAuth();

  return (
    <div className="mb-4 bg-slate-50 p-8 rounded-xl md:flex justify-between items-center">
      <Title />
      {!!user?.username && isStatusVisible && (
        <div className="flex flex-col items-end gap-1">
          <div className="flex items-center gap-3 mb-2 mr-1">
            {/* <p className="hidden md:block text-gray-700 ">
              hey, <span className="font-semibold">{user.username} </span>{" "}
            </p> */}
            <a href="#faq" className="flex items-center justify-center space-x-1 text-slate-500 hover:text-slate-600">
              <span className="text-sm font-normal">Help</span>
              <RxQuestionMarkCircled className="text-base text-slate-600" />
            </a>
            <div className="relative group">
              <Link to="/user">
                <BiSolidUser className="w-5 h-5 text-slate-600" />
                <div className="absolute text-center whitespace-nowrap bg-slate-600 text-white text-xs rounded py-1 px-4 bottom-100 left-1/2 transform -translate-x-1/2 opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all ease-in-out duration-275">
                  My Apps
                </div>
              </Link>
            </div>
          </div>
          <StatusPill status={currentStatus.status} className="hidden md:flex">
            {currentStatus.message}
          </StatusPill>
        </div>
      )}
    </div>
  );
}

function GithubLoginButton() {
  return (
    <button
      className="button gray flex !text-gray-800 hover:bg-slate-300 shadow-md"
      onClick={() => (window.location.href = gitHubSignInUrl)}
    >
      <AiFillGithub className="w-6 h-6 mr-2" /> Sign in with GitHub
    </button>
  );
}
