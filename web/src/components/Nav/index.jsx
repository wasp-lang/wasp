import Link from "@docusaurus/Link";
import { useRef, useState } from "react";
import { Star } from "react-feather";
import Transition from "../../lib/Transition";
import { DiscordIcon, GitHubIcon, TwitterIcon } from "./SocialIcons";
// import Announcement from "./Announcement";

const Nav = () => {
  const [open, setOpen] = useState(false);

  const Logo = () => (
    <div className="flex flex-shrink-0 items-center">
      <Link to="/">
        <img
          src="img/lp/wasp-logo.webp"
          width={35}
          height={35}
          alt="Wasp Logo"
        />
      </Link>
      <span className="ml-3 text-lg font-semibold text-neutral-700">
        Wasp <span className="text-base text-yellow-500">(beta)</span>
      </span>
    </div>
  );

  const SocialIcon = ({ Icon, url }) => (
    <a
      href={url}
      target="_blank"
      rel="noreferrer"
      className={`hidden border-b-2 border-transparent py-5 text-neutral-700 hover:border-yellow-500 hover:text-yellow-500 hover:opacity-75 lg:flex`}
    >
      <Icon />
    </a>
  );

  const WaspGhStarsExactCount = () => (
    <>
      {/* NOTE(matija): If I put width to 100 or less, the badge gets cut off in half. */}
      <iframe
        src="https://ghbtns.com/github-btn.html?user=wasp-lang&repo=wasp&type=star&count=true"
        frameBorder="0"
        scrolling="0"
        width="101px"
        height="20px"
      ></iframe>
    </>
  );

  // NOTE(matija): this one does not show the exact count, but e.g. 2k.
  const WaspGhStarsShield = () => (
    <img
      alt="GitHub Repo stars"
      src="https://img.shields.io/github/stars/wasp-lang/wasp?style=social"
    />
  );

  const GitHubButton = () => (
    <a
      href="https://github.com/wasp-lang/wasp"
      target="_blank"
      rel="noreferrer"
      className={`group hidden items-center space-x-2 rounded px-2.5 py-1 text-xs transition duration-200 ease-out hover:bg-neutral-200 lg:flex`}
    >
      <div
        className={`flex h-3 w-3 items-center justify-center text-neutral-700 group-hover:h-4 group-hover:w-4 group-hover:text-yellow-500`}
      >
        <Star strokeWidth={2} />
      </div>
      <span className="truncate text-neutral-700">Star us on GitHub</span>
    </a>
  );

  const HamburgerButton = ({ toggleFlyOut }) => (
    <div
      className="absolute inset-y-0 left-0 flex items-center px-2 lg:hidden"
      onClick={() => toggleFlyOut()}
    >
      <button
        className={`inline-flex items-center rounded-md p-2 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-yellow-500`}
      >
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

  const navSidebarRef = useRef(null);

  return (
    <>
      {/* <Announcement /> */}
      <div className="sticky top-0 z-50">
        <div className="absolute top-0 h-full w-full bg-[#f5f4f0] opacity-80"></div>
        <nav className="border-b backdrop-blur-sm">
          <div className="relative mx-auto flex h-16 justify-between lg:container lg:px-16 xl:px-20">
            <HamburgerButton toggleFlyOut={() => setOpen(true)} />
            <div className="flex flex-1 items-center justify-center sm:items-stretch lg:justify-between">
              <div className="flex items-center">
                {" "}
                {/* Navbar left side */}
                <Logo />
                <div className="hidden pl-4 sm:ml-6 sm:space-x-4 lg:flex">
                  {" "}
                  {/* Left items */}
                  {/* Docs */}
                  <Link to="/docs">
                    <span
                      className={`border-b-solid border-b-2 border-transparent px-1 py-5 text-sm font-semibold text-neutral-700 hover:border-yellow-500 hover:text-yellow-500`}
                    >
                      Docs
                    </span>
                  </Link>
                  {/* Blog */}
                  <Link to="/blog">
                    <span
                      className={`border-b-2 border-transparent px-1 py-5 text-sm font-semibold text-neutral-700 hover:border-yellow-500 hover:text-yellow-500`}
                    >
                      Blog
                    </span>
                  </Link>
                  {/* FAQ */}
                  <Link to="#faq">
                    <span
                      className={`border-b-2 border-transparent px-1 py-5 text-sm font-medium text-neutral-700 hover:border-yellow-500 hover:text-yellow-500`}
                    >
                      FAQ
                    </span>
                  </Link>
                  {/* Join newsletter */}
                  <Link to="#signup">
                    <span
                      className={`border-b-2 border-transparent px-1 py-5 text-sm font-medium`}
                    >
                      <span
                        className={`rounded bg-yellow-500/25 px-2 py-1 text-neutral-700 hover:bg-yellow-500/10`}
                      >
                        📬 Join the list
                      </span>
                    </span>
                  </Link>
                </div>{" "}
                {/* EOF left items */}
              </div>{" "}
              {/* EOF left side */}
              <div className="flex items-center gap-2 space-x-2">
                {" "}
                {/* Navbar right side */}
                {/* GH stars badge */}
                {/*
                <span className='hidden lg:inline'>
                  <Link to='https://github.com/wasp-lang/wasp' className='flex items-center'>
                    <WaspGhStarsExactCount />
                  </Link>
                </span>
                */}
                <GitHubButton />
                <Link to="/docs/quick-start">
                  <button
                    className={`hidden rounded bg-yellow-500 px-2.5 py-1 text-xs text-white transition duration-200 ease-out hover:bg-yellow-400 lg:block`}
                  >
                    Get Started
                  </button>
                </Link>
                <SocialIcon
                  Icon={GitHubIcon}
                  url="https://github.com/wasp-lang/wasp"
                />
                <SocialIcon
                  Icon={DiscordIcon}
                  url="https://discord.gg/rzdnErX"
                />
                <SocialIcon
                  Icon={TwitterIcon}
                  url="https://twitter.com/WaspLang"
                />
              </div>{" "}
              {/* EOF right side */}
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
              className={`fixed -inset-y-0 z-50 h-screen w-screen transform overflow-y-scroll bg-white p-4 md:p-8`}
            >
              <div className="absolute right-4 top-4 items-center justify-between">
                <div className="-mr-2">
                  <button
                    type="button"
                    onClick={() => setOpen(false)}
                    className={`inline-flex items-center justify-center rounded-md bg-white p-2 text-neutral-700 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset`}
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
              </div>

              <div className="mb-12 mt-6">
                {/* Docs */}
                <div className="space-y-1 pb-4 pt-2">
                  <Link to="/docs">
                    <span className="block pl-3 pr-4 text-base font-medium text-neutral-700">
                      Docs
                    </span>
                  </Link>
                </div>

                {/* Docs */}
                <div className="space-y-1 pb-4 pt-2">
                  <Link to="/blog">
                    <span className="block pl-3 pr-4 text-base font-medium text-neutral-700">
                      Blog
                    </span>
                  </Link>
                </div>

                {/* FAQ */}
                <div className="space-y-1 pb-4 pt-2">
                  <Link to="#faq" onClick={() => setOpen(false)}>
                    <span className="block pl-3 pr-4 text-base font-medium text-neutral-700">
                      FAQ
                    </span>
                  </Link>
                </div>

                {/* Join the list */}
                <div className="space-y-1 pb-4 pt-2">
                  <Link to="#signup" onClick={() => setOpen(false)}>
                    <span
                      className={`block rounded bg-yellow-500/25 px-2 py-1 pl-3 pr-4 text-base font-medium text-neutral-700 hover:bg-yellow-500/10`}
                    >
                      📬 Join the list
                    </span>
                  </Link>
                </div>

                {/* GitHub */}
                <div className="space-y-1 pb-4 pt-2">
                  <Link to="https://github.com/wasp-lang/wasp">
                    <span className="flex items-center">
                      <span className="pl-3 pr-4 text-base font-medium text-neutral-700">
                        ⭐️ GitHub
                      </span>
                      <WaspGhStarsExactCount />
                    </span>
                  </Link>
                </div>

                {/* Discord */}
                <div className="space-y-1 pb-4 pt-2">
                  <Link to="https://discord.gg/rzdnErX">
                    <span className="block pl-3 pr-4 text-base font-medium text-neutral-700">
                      👾 Discord
                    </span>
                  </Link>
                </div>

                {/* Twitter */}
                <div className="space-y-1 pb-4 pt-2">
                  <Link to="https://twitter.com/WaspLang">
                    <span className="block pl-3 pr-4 text-base font-medium text-neutral-700">
                      🐦 Twitter
                    </span>
                  </Link>
                </div>
              </div>
            </div>
          </Transition>
        </nav>
      </div>
    </>
  );
};

export default Nav;
