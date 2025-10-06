import { useState } from "react";
import { useLocation } from "react-router-dom";
import { Link, type Routes } from "wasp/client/router";
import { cn } from "../cn";

export function FeatureContainer({ children }: React.PropsWithChildren<{}>) {
  return (
    <div className="z-50 flex">
      <FeatureListMenu />

      <main className="flex-1 lg:ml-0">
        <div className="p-6 lg:p-8">{children}</div>
      </main>
    </div>
  );
}

function FeatureListMenu() {
  return (
    <>
      <DesktopFeatureListMenu />

      <MobileFeatureListMenu />
    </>
  );
}

function DesktopFeatureListMenu() {
  return (
    <div className="hidden w-80 overflow-y-auto border-r border-gray-200 bg-white lg:block">
      <div className="p-6">
        <nav className="space-y-2">
          {features.map((feature) => (
            <FeatureCard {...feature} key={feature.to} />
          ))}
        </nav>
      </div>
    </div>
  );
}

function MobileFeatureListMenu() {
  const [isMobileMenuOpen, setIsMobileMenuOpen] = useState(false);

  return (
    <>
      <div
        className={cn(
          "fixed inset-x-0 bottom-0 z-40 border-t border-gray-200 bg-white transition-transform duration-300 lg:hidden",
          isMobileMenuOpen ? "translate-y-0" : "translate-y-full",
        )}
      >
        <div className="overflow-y-auto p-6">
          <nav className="space-y-2">
            {features.map((feature) => (
              <FeatureCard {...feature} key={feature.to} />
            ))}
          </nav>
        </div>
      </div>

      {isMobileMenuOpen && (
        <div
          className="fixed inset-0 z-30 bg-black bg-opacity-50 lg:hidden"
          onClick={() => setIsMobileMenuOpen(false)}
        />
      )}

      <button
        onClick={() => setIsMobileMenuOpen(!isMobileMenuOpen)}
        className="fixed bottom-6 right-6 z-50 rounded-xl border border-gray-200 bg-white p-3 shadow-lg lg:hidden"
      >
        <div className="flex items-center gap-2 text-gray-700">
          <svg
            className="h-6 w-6"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M4 6h16M4 12h16M4 18h16"
            />
          </svg>
          <span>Features</span>
        </div>
      </button>
    </>
  );
}

type Feature = {
  title: string;
  isPublic?: boolean;
  to: string;
} & Routes;

const features: Feature[] = [
  {
    to: "/tasks",
    title: "Operations",
  },
  {
    to: "/custom-signup",
    title: "Custom Signup",
    isPublic: true,
  },
  {
    to: "/manual-signup",
    title: "Manual Signup",
    isPublic: true,
  },
  {
    to: "/apis",
    title: "Custom APIs",
    isPublic: true,
  },
  {
    to: "/jobs",
    title: "Async Jobs",
  },
  {
    to: "/chat",
    title: "Websockets",
  },
  {
    to: "/crud",
    title: "Automatic CRUD",
  },
  {
    to: "/streaming",
    title: "Streaming",
  },
  {
    to: "/serialization",
    title: "Serialization Test",
    isPublic: true,
  },
  {
    to: "/profile",
    title: "User Profile",
  },
];

function FeatureCard({ title, isPublic, ...routeProps }: Feature) {
  const location = useLocation();
  const isActive = location.pathname.startsWith(routeProps.to);

  return (
    <Link
      {...routeProps}
      className={cn(
        "group relative block rounded-lg border border-transparent p-3 transition-all duration-200 hover:bg-gray-50",
        isActive
          ? "border-gray-200 bg-gray-50 text-gray-900 hover:bg-gray-50"
          : "text-gray-700 hover:text-gray-900",
      )}
    >
      <div className="flex items-center gap-2">
        <h3 className="text-sm font-medium leading-tight">{title}</h3>
        {isPublic && (
          <span className="inline-flex items-center rounded bg-green-50 px-1.5 py-0.5 text-xs font-medium text-green-700">
            Public
          </span>
        )}
      </div>
    </Link>
  );
}
