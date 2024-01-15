import { Title } from "./Title";
import { BiSolidUser } from "react-icons/bi";
import { RxQuestionMarkCircled } from "react-icons/rx";
import useAuth from "@wasp/auth/useAuth";
import { useHistory } from "react-router-dom";
import { BiSolidHome } from "react-icons/bi";
import { Link } from "@wasp/router";
import { StatusPill } from "./StatusPill";

export function Header({ children, currentStatus }) {
  return (
    <div className="mb-4 bg-slate-50 p-8 rounded-xl md:flex justify-between items-center">
      <Title />
      <div className="flex flex-col items-end gap-2">
        <div className="flex items-center gap-3 m-1">{children}</div>
        {!!currentStatus && (
          <StatusPill status={currentStatus.status} className="hidden md:flex">
            {currentStatus.message}
          </StatusPill>
        )}
      </div>
    </div>
  );
}

export function FaqButton() {
  return (
    <a href="#faq" className="flex items-center justify-center space-x-1 text-slate-500 hover:text-slate-600">
      <span className="text-sm font-normal">Help</span>
      <RxQuestionMarkCircled className="text-base text-slate-600" />
    </a>
  );
}

export function ProfileButton({ setIsLoginModalOpen }) {
  const { data: user } = useAuth();
  const history = useHistory();

  return (
    <button
      className="relative group"
      onClick={() => {
        if (!user) {
          setIsLoginModalOpen(true);
        } else {
          history.push("/user");
        }
      }}
    >
      <BiSolidUser className="w-5 h-5 text-slate-600" />
      <ToolTip>Profile</ToolTip>
    </button>
  );
}

export function HomeButton() {
  return (
    <button className="relative group">
      <Link to="/">
        <BiSolidHome className="w-5 h-5 text-slate-600" />
        <ToolTip>Home</ToolTip>
      </Link>
    </button>
  );
}

function ToolTip({ children }) {
  return (
    <div className="absolute text-center whitespace-nowrap bg-slate-600 text-white text-xs rounded py-1 px-4 bottom-100 left-1/2 transform -translate-x-1/2 translate-y-1 opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all ease-in-out duration-275">
      {children}
    </div>
  );
}
