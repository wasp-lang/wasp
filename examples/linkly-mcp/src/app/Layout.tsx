import { Outlet, Link as RouterLink, useLocation } from "react-router";
import { logout, useAuth } from "wasp/client/auth";
import { Button } from "../components/ui/button";
import { Toaster } from "../components/ui/sonner";
import { cn } from "../lib/utils";
import "../Main.css";

const NAV = [
  { to: "/links", label: "Links" },
  { to: "/analytics", label: "Top links" },
  { to: "/docs", label: "Docs" },
  { to: "/settings/connections", label: "Connections" },
];

export function Layout() {
  const { data: user } = useAuth();
  const { pathname } = useLocation();

  return (
    <div className="bg-background text-foreground min-h-screen">
      <header className="border-b">
        <div className="mx-auto flex h-14 max-w-6xl items-center gap-6 px-4">
          <RouterLink
            to={user ? "/links" : "/"}
            className="font-heading text-lg font-semibold"
          >
            Linkly
          </RouterLink>
          {user && (
            <nav
              className="flex flex-1 items-center gap-1 overflow-x-auto text-sm"
              aria-label="Main"
            >
              {NAV.map((item) => (
                <RouterLink
                  key={item.to}
                  to={item.to}
                  className={cn(
                    "text-muted-foreground hover:text-foreground rounded-md px-3 py-1.5 whitespace-nowrap",
                    pathname.startsWith(item.to) && "bg-muted text-foreground",
                  )}
                >
                  {item.label}
                </RouterLink>
              ))}
            </nav>
          )}
          <div className="ml-auto flex items-center gap-2">
            {user ? (
              <Button variant="ghost" size="sm" onClick={() => logout()}>
                Log out
              </Button>
            ) : (
              <>
                <Button variant="ghost" size="sm" asChild>
                  <RouterLink to="/login">Log in</RouterLink>
                </Button>
                <Button size="sm" asChild>
                  <RouterLink to="/signup">Sign up</RouterLink>
                </Button>
              </>
            )}
          </div>
        </div>
      </header>
      {/* Extra bottom padding so the badge never covers the end of a page. */}
      <main className="mx-auto max-w-6xl px-4 pt-8 pb-20">
        <Outlet />
      </main>
      <a
        href="https://wasp.sh"
        target="_blank"
        rel="noopener noreferrer"
        className="bg-background text-muted-foreground hover:text-foreground fixed right-4 bottom-4 z-40 rounded-full border px-3 py-1.5 text-xs transition-colors"
      >
        <span className="">
          {"{"}= made with wasp ={"}"}
        </span>
      </a>
      <Toaster />
    </div>
  );
}
