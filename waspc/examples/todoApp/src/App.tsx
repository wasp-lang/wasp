import { Outlet } from "react-router-dom";

import { AuthUser } from "wasp/auth";
import { env } from "wasp/client";
import { logout, useAuth } from "wasp/client/auth";
import { getDate, useQuery } from "wasp/client/operations";
import { Link } from "wasp/client/router";
import { useSocket } from "wasp/client/webSocket";

import "./Main.css";
import { cn } from "./cn";
import { Button, ButtonLink } from "./components/Button";
import { getName } from "./features/auth/user";

// Necessary to trigger type tests.
import "./rpcTests/operations/client";

export function App() {
  const { data: user } = useAuth();

  return (
    <div className="grid grid-rows-[auto_1fr_auto] min-h-screen bg-gray-50">
      <Header user={user} />

      <Outlet />

      <Footer />
    </div>
  );
}

function Header({ user }: { user: AuthUser | null | undefined }) {
  const appName = env.REACT_APP_NAME;

  return (
    <header className="bg-white/80 backdrop-blur-sm border-b border-gray-200 sticky top-0 z-40">
      <div className="container mx-auto px-4 py-4">
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-4">
            <h1 className="text-2xl md:text-3xl font-bold text-gray-900">
              <Link
                to="/"
                className="hover:text-primary-600 transition-colors duration-200"
              >
                {appName}
              </Link>
            </h1>
          </div>

          <div className="flex items-center space-x-4">
            {user ? (
              <UserMenu user={user} />
            ) : (
              <div className="flex items-center space-x-3">
                <ButtonLink to="/login" variant="ghost">
                  Sign In
                </ButtonLink>
                <ButtonLink to="/signup" variant="primary">
                  Sign Up
                </ButtonLink>
              </div>
            )}
          </div>
        </div>
      </div>
    </header>
  );
}

function Footer() {
  const { data: date } = useQuery(getDate);
  const { isConnected } = useSocket();

  return (
    <footer className="bg-white border-t border-gray-200">
      <div className="container mx-auto px-4 py-8">
        <div className="flex flex-col md:flex-row items-center justify-between space-y-4 md:space-y-0">
          <div className="flex items-center space-x-2">
            <span className="text-gray-600">Created with</span>
            <a
              href="https://wasp.sh"
              target="_blank"
              rel="noopener noreferrer"
              className="link"
            >
              Wasp
            </a>
          </div>

          <StatusInfo date={date} isConnected={isConnected} />

          <div className="flex items-center space-x-6 text-sm text-gray-500">
            <div className="flex space-x-4">
              <a href="https://wasp.sh/docs" className="link" target="_blank">
                Docs
              </a>
              <a
                href="https://discord.com/invite/rzdnErX"
                className="link"
                target="_blank"
              >
                Discord
              </a>
            </div>
          </div>
        </div>
      </div>
    </footer>
  );
}

function UserMenu({ user }: { user: AuthUser }) {
  const name = getName(user);
  return (
    <div className="flex items-center space-x-4">
      <div className="hidden md:flex items-center space-x-2">
        <UserAvatar user={user} />
        <div className="flex flex-col">
          <span className="text-sm text-gray-600">Welcome back,</span>
          <Link
            to="/profile"
            className="text-sm font-medium text-gray-900 hover:text-primary-600 transition-colors"
          >
            {name}
          </Link>
        </div>
      </div>

      <Link to="/profile">
        <UserAvatar user={user} className="md:hidden" />
      </Link>

      <Button onClick={logout} variant="secondary">
        Sign Out
      </Button>
    </div>
  );
}

function UserAvatar({
  user,
  className,
}: {
  user: AuthUser;
  className?: string;
}) {
  const name = getName(user);
  return (
    <div
      className={cn(
        "w-8 h-8 bg-primary-100 border border-primary-300 rounded-full flex items-center justify-center",
        className,
      )}
    >
      <span className="text-primary-600 font-medium text-sm">
        {name?.charAt(0).toUpperCase()}
      </span>
    </div>
  );
}

function StatusInfo({
  date,
  isConnected,
}: {
  date: Date | undefined;
  isConnected: boolean;
}) {
  return (
    <div className="flex items-center space-x-4 text-sm">
      <div className="flex items-center space-x-2">
        <div
          className={cn(
            "w-2 h-2 rounded-full",
            isConnected && "bg-green-500",
            !isConnected && "bg-red-500",
          )}
        />
        <span className="text-gray-600">
          {isConnected ? "Connected" : "Disconnected"}
        </span>
      </div>

      {date && (
        <div className="text-gray-500">Loaded {date.toLocaleString()}</div>
      )}
    </div>
  );
}
