import Link from "@docusaurus/Link";
import { useRef, useState } from "react";
import Transition from "../../lib/Transition";
import Announcement from "./Announcement";
import Logo from "./Logo";

const Nav = () => {
  const [open, setOpen] = useState(false);
  const navSidebarRef = useRef(null);

  const navLinkClass =
    "font-mono text-sm font-medium text-wasp-g6 tracking-wide hover:text-wasp-black transition-colors";

  const mobileLinkClass =
    "block pl-3 font-mono text-base font-medium text-wasp-g6";

  const closeMenu = () => setOpen(false);

  const HamburgerButton = ({ toggleFlyOut }) => (
    <div
      className="absolute inset-y-0 left-0 flex items-center px-2 lg:hidden"
      onClick={toggleFlyOut}
    >
      <button className="inline-flex items-center p-2 hover:bg-wasp-g1 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-wasp-yellow">
        <span className="sr-only">Open menu</span>
        <svg
          className="block h-6 w-6"
          xmlns="http://www.w3.org/2000/svg"
          fill="none"
          viewBox="0 0 24 24"
          stroke="currentColor"
          aria-hidden="true"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth="2"
            d="M4 6h16M4 12h16M4 18h16"
          />
        </svg>
      </button>
    </div>
  );

  return (
    <>
      <Announcement />
      <div className="sticky top-0 z-50">
        <nav className="border-b border-wasp-g3 bg-wasp-bg">
          <div className="relative mx-auto flex h-16 items-center justify-between lg:container lg:px-16">
            <HamburgerButton toggleFlyOut={() => setOpen(true)} />

            {/* Left: Logo */}
            <div className="flex flex-1 items-center justify-center lg:justify-start">
              <Logo />
            </div>

            {/* Right: Nav links + CTA */}
            <div className="hidden items-center gap-7 lg:flex">
              <Link to="/docs" className={navLinkClass}>
                Docs
              </Link>
              <Link to="/blog" className={navLinkClass}>
                Blog
              </Link>
              <a
                href="https://github.com/wasp-lang/wasp"
                target="_blank"
                rel="noreferrer"
                className={navLinkClass}
              >
                GitHub (18k)
              </a>
              <a
                href="https://discord.gg/rzdnErX"
                target="_blank"
                rel="noreferrer"
                className={navLinkClass}
              >
                Discord
              </a>
              <Link
                to="/docs/quick-start"
                className="inline-flex items-center border-2 border-wasp-black bg-wasp-yellow px-4 py-1.5 font-mono text-xs font-bold uppercase tracking-widest text-wasp-black transition-colors hover:bg-wasp-yellow-dark hover:text-wasp-black"
              >
                Get Started
              </Link>
            </div>
          </div>

          {/* Mobile Nav Menu */}
          <Transition
            nodeRef={navSidebarRef}
            appear={true}
            show={open}
            enter="transition ease-out duration-200"
            enterFrom="opacity-0 translate-y-1"
            enterTo="opacity-100 translate-y-0"
            leave="transition ease-in duration-150"
            leaveFrom="opacity-100 translate-y-0"
            leaveTo="opacity-0 translate-y-1"
          >
            <div
              ref={navSidebarRef}
              className="fixed inset-y-0 z-50 h-screen w-screen transform overflow-y-scroll bg-wasp-bg p-4 md:p-8"
            >
              <div className="absolute right-4 top-4">
                <button
                  type="button"
                  onClick={closeMenu}
                  className="inline-flex items-center justify-center p-2 text-wasp-g6 hover:bg-wasp-g1 focus:outline-none"
                >
                  <span className="sr-only">Close menu</span>
                  <svg
                    className="h-6 w-6"
                    xmlns="http://www.w3.org/2000/svg"
                    fill="none"
                    viewBox="0 0 24 24"
                    stroke="currentColor"
                    aria-hidden="true"
                  >
                    <path
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      strokeWidth="2"
                      d="M6 18L18 6M6 6l12 12"
                    />
                  </svg>
                </button>
              </div>

              <div className="mb-12 mt-6 space-y-6">
                <Link
                  to="/docs"
                  onClick={closeMenu}
                  className={mobileLinkClass}
                >
                  Docs
                </Link>
                <Link
                  to="/blog"
                  onClick={closeMenu}
                  className={mobileLinkClass}
                >
                  Blog
                </Link>
                <a
                  href="https://github.com/wasp-lang/wasp"
                  target="_blank"
                  rel="noreferrer"
                  onClick={closeMenu}
                  className={mobileLinkClass}
                >
                  GitHub (18k)
                </a>
                <a
                  href="https://discord.gg/rzdnErX"
                  target="_blank"
                  rel="noreferrer"
                  onClick={closeMenu}
                  className={mobileLinkClass}
                >
                  Discord
                </a>
                <Link
                  to="/docs/quick-start"
                  onClick={closeMenu}
                  className="ml-3 mt-2 inline-flex items-center border-2 border-wasp-black bg-wasp-yellow px-4 py-2 font-mono text-xs font-bold uppercase tracking-widest text-wasp-black transition-colors hover:bg-wasp-yellow-dark hover:text-wasp-black"
                >
                  Get Started
                </Link>
              </div>
            </div>
          </Transition>
        </nav>
      </div>
    </>
  );
};

export default Nav;
