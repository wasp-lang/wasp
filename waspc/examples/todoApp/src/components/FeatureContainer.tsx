import { useState } from "react";
import { useLocation } from "react-router-dom";
import { Link, type Routes } from "wasp/client/router";
import { cn } from "../cn";

export function FeatureContainer({ children }: React.PropsWithChildren<{}>) {
  return (
    <div className="flex z-50">
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
    <div className="hidden lg:block w-80 bg-white border-r border-gray-200 overflow-y-auto">
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
          "fixed inset-x-0 bottom-0 z-40 bg-white border-t border-gray-200 lg:hidden transition-transform duration-300",
          isMobileMenuOpen ? "translate-y-0" : "translate-y-full",
        )}
      >
        <div className="p-6 overflow-y-auto">
          <nav className="space-y-2">
            {features.map((feature) => (
              <FeatureCard {...feature} key={feature.to} />
            ))}
          </nav>
        </div>
      </div>

      {isMobileMenuOpen && (
        <div
          className="fixed inset-0 bg-black bg-opacity-50 z-30 lg:hidden"
          onClick={() => setIsMobileMenuOpen(false)}
        />
      )}

      <button
        onClick={() => setIsMobileMenuOpen(!isMobileMenuOpen)}
        className="fixed bottom-6 right-6 z-50 lg:hidden bg-white p-3 rounded-xl shadow-lg border border-gray-200"
      >
        <div className="flex items-center gap-2 text-gray-700">
          <svg
            className="w-6 h-6"
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
        "group relative block p-3 rounded-lg transition-all duration-200 hover:bg-gray-50 border border-transparent",
        isActive
          ? "bg-gray-50 border-gray-200 text-gray-900 hover:bg-gray-50"
          : "text-gray-700 hover:text-gray-900",
      )}
    >
      <div className="flex items-center gap-2">
        <h3 className="font-medium text-sm leading-tight">{title}</h3>
        {isPublic && (
          <span className="inline-flex items-center px-1.5 py-0.5 rounded text-xs font-medium bg-green-50 text-green-700">
            Public
          </span>
        )}
      </div>
    </Link>
  );
}
