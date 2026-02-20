import { BiSolidHome, BiSolidUser } from "react-icons/bi";
import { RxQuestionMarkCircled } from "react-icons/rx";
import { useNavigate } from "react-router";

import { useAuth } from "wasp/client/auth";
import { Link } from "wasp/client/router";

import { StatusPill } from "./StatusPill";
import { Title } from "./Title";

export function Header({ children, currentStatus }) {
  return (
    <div className="mb-4 items-center justify-between rounded-xl bg-slate-50 p-8 md:flex">
      <Title />
      <div className="flex flex-col items-end gap-2">
        <div className="m-1 flex items-center gap-3">{children}</div>
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
    <a
      href="#faq"
      className="flex items-center justify-center space-x-1 text-slate-500 hover:text-slate-600"
    >
      <span className="text-sm font-normal">Help</span>
      <RxQuestionMarkCircled className="text-base text-slate-600" />
    </a>
  );
}

export function ProfileButton({ setIsLoginModalOpen }) {
  const { data: user } = useAuth();
  const navigate = useNavigate();

  return (
    <button
      className="group relative"
      onClick={() => {
        if (!user) {
          setIsLoginModalOpen(true);
        } else {
          navigate("/user");
        }
      }}
    >
      <BiSolidUser className="h-5 w-5 text-slate-600" />
      <ToolTip>Profile</ToolTip>
    </button>
  );
}

export function HomeButton() {
  return (
    <button className="group relative">
      <Link to="/">
        <BiSolidHome className="h-5 w-5 text-slate-600" />
        <ToolTip>Home</ToolTip>
      </Link>
    </button>
  );
}

function ToolTip({ children }) {
  return (
    <div className="invisible absolute bottom-100 left-1/2 -translate-x-1/2 translate-y-1 transform rounded-sm bg-slate-600 px-4 py-1 text-center text-xs whitespace-nowrap text-white opacity-0 transition-all duration-275 ease-in-out group-hover:visible group-hover:opacity-100">
      {children}
    </div>
  );
}
