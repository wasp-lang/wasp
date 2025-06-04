import { Outlet } from "react-router-dom";
import { env } from "wasp/client";
import { logout, useAuth } from "wasp/client/auth";
import { getDate, useQuery } from "wasp/client/operations";
import { Link } from "wasp/client/router";
import { useSocket } from "wasp/client/webSocket";

import "./Main.css";
import { getName } from "./features/auth/user";
// Necessary to trigger type tests.
import { AuthUser } from "wasp/auth";
import { cn } from "./cn";
import { Button } from "./components/Button";
import "./rpcTests/operations/client";

export function App() {
  const { data: user } = useAuth();
  const { data: date } = useQuery(getDate);
  const { isConnected } = useSocket();

  const appName = env.REACT_APP_NAME;

  return (
    <div className="grid grid-rows-[auto_1fr_auto] min-h-screen bg-gray-50">
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
                  <Button to="/login" variant="ghost">
                    Sign In
                  </Button>
                  <Button to="/signup" variant="primary">
                    Sign Up
                  </Button>
                </div>
              )}
            </div>
          </div>
        </div>
      </header>

      <Outlet />

      <footer className="bg-white border-t border-gray-200">
        <div className="container mx-auto px-4 py-8">
          <div className="flex flex-col md:flex-row items-center justify-between space-y-4 md:space-y-0">
            <div className="flex items-center space-x-2">
              <span className="text-gray-600">Created with</span>
              <a
                href="https://wasp.sh"
                target="_blank"
                rel="noopener noreferrer"
                className="font-semibold text-primary-600 hover:text-primary-700 transition-colors"
              >
                Wasp
              </a>
              <span className="text-lg">🐝</span>
            </div>

            <StatusInfo date={date} isConnected={isConnected} />

            <div className="flex items-center space-x-6 text-sm text-gray-500">
              <span>© 2025</span>
              <div className="flex space-x-4">
                <a
                  href="https://wasp.sh/docs"
                  className="hover:text-gray-700 transition-colors"
                  target="_blank"
                >
                  Docs
                </a>
                <a
                  href="https://discord.com/invite/rzdnErX"
                  className="hover:text-gray-700 transition-colors"
                  target="_blank"
                >
                  Discord
                </a>
              </div>
            </div>
          </div>
        </div>
      </footer>
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
            isConnected ? "bg-green-500" : "bg-red-500",
          )}
        />
        <span className="text-gray-600">
          {isConnected ? "Connected" : "Disconnected"}
        </span>
      </div>

      {date && (
        <div className="flex items-center space-x-2 text-gray-500">
          <svg
            className="w-4 h-4"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"
            />
          </svg>
          <span>Loaded {date.toLocaleString()}</span>
        </div>
      )}
    </div>
  );
}

function UserMenu({ user }: { user: AuthUser }) {
  return (
    <div className="flex items-center space-x-4">
      <div className="hidden md:flex items-center space-x-2">
        <div className="w-8 h-8 bg-primary-100 rounded-full flex items-center justify-center">
          <span className="text-primary-600 font-medium text-sm">
            {getName(user)?.charAt(0).toUpperCase()}
          </span>
        </div>
        <div className="flex flex-col">
          <span className="text-sm text-gray-600">Welcome back,</span>
          <Link
            to="/profile"
            className="text-sm font-medium text-gray-900 hover:text-primary-600 transition-colors"
          >
            {getName(user)}
          </Link>
        </div>
      </div>

      <Link
        to="/profile"
        className="md:hidden w-8 h-8 bg-primary-100 rounded-full flex items-center justify-center hover:bg-primary-200 transition-colors"
      >
        <span className="text-primary-600 font-medium text-sm">
          {getName(user)?.charAt(0).toUpperCase()}
        </span>
      </Link>

      <Button onClick={logout} variant="secondary">
        Sign Out
      </Button>
    </div>
  );
}
