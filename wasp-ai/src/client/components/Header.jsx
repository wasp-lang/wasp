import { StatusPill } from "./StatusPill";
import { Title } from "./Title";
import { BiSolidUser, BiSolidHome } from "react-icons/bi";
import { RxQuestionMarkCircled } from "react-icons/rx";
import useAuth from "@wasp/auth/useAuth";
import { Link } from "@wasp/router";

export function Header({ currentStatus, isStatusVisible, isUserPage, setIsLoginModalOpen }) {
  const { data: user } = useAuth();

  return (
    <div className="mb-4 bg-slate-50 p-8 rounded-xl md:flex justify-between items-center">
      <Title />
      {isStatusVisible && (
        <div className="flex flex-col items-end gap-2">
          <div className="flex items-center gap-3 my-1 mr-1">
            <a href="#faq" className="flex items-center justify-center space-x-1 text-slate-500 hover:text-slate-600">
              <span className="text-sm font-normal">Help</span>
              <RxQuestionMarkCircled className="text-base text-slate-600" />
            </a>

            <div className="flex items-center relative group">
              <button
                onClick={() => {
                  if (!user) {
                    setIsLoginModalOpen(true);
                  } else {
                   window.location.href = "/user";
                  }
                }}
              >
                <BiSolidUser className="w-5 h-5 text-slate-600" />
                <div className="absolute text-center whitespace-nowrap bg-slate-600 text-white text-xs rounded py-1 px-4 bottom-100 left-1/2 transform -translate-x-1/2 translate-y-1 opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all ease-in-out duration-275">
                  My Apps
                </div>
              </button>
            </div>
          </div>
          <StatusPill status={currentStatus.status} className="hidden md:flex">
            {currentStatus.message}
          </StatusPill>
        </div>
      )}
      {isUserPage && (
        <div className="flex items-center justify-center gap-3 mr-2">
          <button className="relative group">
            <Link to="/">
              <BiSolidHome className="w-5 h-5 text-slate-600 mr-1" />
              <div className="absolute text-center whitespace-nowrap bg-slate-600 text-white text-xs rounded py-1 px-4 bottom-100 left-1/2 transform -translate-x-1/2 translate-y-1 opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all ease-in-out duration-275">
                Home
              </div>
            </Link>
          </button>
        </div>
      )}
    </div>
  );
}
