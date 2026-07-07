import { useEffect, useState } from "react";
import { useLocation } from "react-router";
import { NavLink, type Routes } from "wasp/client/router";
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
        <FeatureTree />
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
          "fixed inset-x-0 bottom-0 z-40 max-h-[calc(100dvh-4rem)] overflow-y-auto border-t border-gray-200 bg-white transition-transform duration-300 lg:hidden",
          isMobileMenuOpen ? "translate-y-0" : "translate-y-full",
        )}
      >
        <div className="px-8 pt-8 pb-28">
          <FeatureTree />
        </div>
      </div>

      {isMobileMenuOpen && (
        <div
          className="bg-opacity-50 fixed inset-0 z-30 bg-black lg:hidden"
          onClick={() => setIsMobileMenuOpen(false)}
        />
      )}

      <button
        onClick={() => setIsMobileMenuOpen(!isMobileMenuOpen)}
        className="fixed right-6 bottom-6 z-50 rounded-xl border border-gray-200 bg-white p-3 shadow-lg lg:hidden"
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

type FeatureGroup = {
  title: string;
  features: Feature[];
};

const featureGroups: FeatureGroup[] = [
  {
    title: "Data & Operations",
    features: [
      {
        to: "/tasks",
        title: "Operations",
      },
      {
        to: "/crud",
        title: "Automatic CRUD",
      },
      {
        to: "/serialization",
        title: "Serialization Test",
        isPublic: true,
      },
    ],
  },
  {
    title: "Server Features",
    features: [
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
        to: "/streaming",
        title: "Streaming",
      },
    ],
  },
  {
    title: "Auth & Account",
    features: [
      {
        to: "/profile",
        title: "User Profile",
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
    ],
  },
  {
    title: "Routing & Rendering",
    features: [
      {
        to: "/lazy/no",
        title: "Eager Route",
        isPublic: true,
      },
      {
        to: "/lazy/yes",
        title: "Lazy Route",
        isPublic: true,
      },
      {
        to: "/prerender",
        title: "Prerendering",
        isPublic: true,
      },
      {
        to: "/hydration-mismatch",
        title: "Hydration Mismatch",
        isPublic: true,
      },
    ],
  },
  {
    title: "Other",
    features: [
      {
        to: "/fsm",
        title: "Full-stack Module",
        isPublic: true,
      },
    ],
  },
];

function FeatureTree() {
  const location = useLocation();
  const activeGroup = getFeatureGroupForPath(location.pathname);
  const [openGroups, setOpenGroups] = useState(() =>
    getInitiallyOpenGroups(location.pathname),
  );

  useEffect(() => {
    if (!activeGroup) {
      return;
    }

    setOpenGroups((currentOpenGroups) => {
      if (currentOpenGroups.has(activeGroup.title)) {
        return currentOpenGroups;
      }

      return new Set(currentOpenGroups).add(activeGroup.title);
    });
  }, [activeGroup]);

  function toggleGroup(title: string) {
    setOpenGroups((currentOpenGroups) => {
      const nextOpenGroups = new Set(currentOpenGroups);

      if (nextOpenGroups.has(title)) {
        nextOpenGroups.delete(title);
      } else {
        nextOpenGroups.add(title);
      }

      return nextOpenGroups;
    });
  }

  return (
    <nav aria-label="Features" className="space-y-4">
      {featureGroups.map((group) => {
        const isOpen = openGroups.has(group.title);

        return (
          <section className="space-y-1" key={group.title}>
            <button
              type="button"
              aria-expanded={isOpen}
              className="flex w-full items-center gap-2 rounded-lg px-2 py-1.5 text-left text-xs font-semibold tracking-wide text-gray-500 uppercase transition-colors hover:bg-gray-50 hover:text-gray-700"
              onClick={() => toggleGroup(group.title)}
            >
              <svg
                className={cn(
                  "h-3.5 w-3.5 transition-transform duration-200",
                  isOpen && "rotate-90",
                )}
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="m9 5 7 7-7 7"
                />
              </svg>
              <span>{group.title}</span>
            </button>

            {isOpen && (
              <div className="ml-3 space-y-1 border-l border-gray-200 pl-3">
                {group.features.map((feature) => (
                  <FeatureCard {...feature} key={feature.to} />
                ))}
              </div>
            )}
          </section>
        );
      })}
    </nav>
  );
}

function getInitiallyOpenGroups(pathname: string) {
  const activeGroup = getFeatureGroupForPath(pathname);

  if (activeGroup) {
    return new Set([activeGroup.title]);
  }

  return new Set(featureGroups[0] ? [featureGroups[0].title] : []);
}

function getFeatureGroupForPath(pathname: string) {
  return featureGroups.find((group) =>
    group.features.some((feature) => isFeaturePathActive(pathname, feature.to)),
  );
}

function isFeaturePathActive(pathname: string, featurePath: string) {
  return pathname === featurePath || pathname.startsWith(`${featurePath}/`);
}

function FeatureCard({ title, isPublic, ...routeProps }: Feature) {
  return (
    <NavLink
      {...routeProps}
      className={({ isActive }) =>
        cn(
          "group relative block rounded-lg border border-transparent p-3 transition-all duration-200 hover:bg-gray-50",
          isActive
            ? "border-gray-200 bg-gray-50 text-gray-900 hover:bg-gray-50"
            : "text-gray-700 hover:text-gray-900",
        )
      }
    >
      <div className="flex items-center gap-2">
        <h3 className="text-sm leading-tight font-medium">{title}</h3>
        {isPublic && (
          <span className="inline-flex items-center rounded-sm bg-green-50 px-1.5 py-0.5 text-xs font-medium text-green-700">
            Public
          </span>
        )}
      </div>
    </NavLink>
  );
}
