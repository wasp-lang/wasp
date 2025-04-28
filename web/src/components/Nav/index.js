import React, { useState, useEffect } from 'react'
import Link from '@docusaurus/Link'
import { Star, Twitter } from 'react-feather'

import Announcement from './Announcement'
import Transition from '../../lib/Transition'
import { GitHubIcon, DiscordIcon, TwitterIcon } from './SocialIcons'

const Nav = () => {
  const [open, setOpen] = useState(false)
  const [showLogo, setShowLogo] = useState(true)

  // Scroll listener effect
  useEffect(() => {
    let lastScrollY = window.scrollY
    const handleScroll = () => {
      // Simple check: hide if scrolled down more than 10px
      if (window.scrollY > 10) {
        setShowLogo(false)
      } else {
        setShowLogo(true)
      }
      // Update lastScrollY if needed for direction detection later
      lastScrollY = window.scrollY > 0 ? window.scrollY : 0
    }

    window.addEventListener('scroll', handleScroll, { passive: true })

    return () => {
      window.removeEventListener('scroll', handleScroll)
    }
  }, []) // Run only on mount and unmount

  const Logo = () => (
    <div className="flex flex-shrink-0 items-center whitespace-nowrap">
      <Link to="/">
        <img
          src="img/lp/wasp-logo.webp"
          width={35}
          height={35}
          alt="Wasp Logo"
        />
      </Link>
      <span className={`mx-3 text-3xl text-neutral-700 font-pixelated`}>
         Wasp 
      </span>
    </div>
  )

  const SocialIcon = ({ Icon, url }) => (
    <a
      href={url}
      target="_blank"
      rel="noreferrer"
      className={`
        hidden
        border-b-2 border-transparent py-5 text-neutral-700
        hover:border-yellow-500 hover:text-yellow-500
        hover:opacity-75 lg:flex
      `}
    >
      <Icon />
    </a>
  )

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
  )

  // NOTE(matija): this one does not show the exact count, but e.g. 2k.
  const WaspGhStarsShield = () => (
    <img
      alt="GitHub Repo stars"
      src="https://img.shields.io/github/stars/wasp-lang/wasp?style=social"
    />
  )

  const GitHubButton = () => (
    <a
      href="https://github.com/wasp-lang/wasp"
      target="_blank"
      rel="noreferrer"
      className={`
        hidden items-center rounded-sm bg-neutral-200/70 backdrop-blur-sm 
        p-2 hover:bg-[#ffcc00] hover:text-neutral-800 
        text-neutral-700 font-mono text-xs transition-colors duration-150 lg:flex
      `}
    >
      <span>[G] GITHUB</span> 
    </a>
  )

  const HamburgerButton = ({ toggleFlyOut }) => (
    <div
      className="absolute inset-y-0 left-0 flex items-center px-2 lg:hidden"
      onClick={() => toggleFlyOut()}
    >
      <button
        className={`
          inline-flex items-center rounded-md
          p-2 hover:bg-gray-50
          focus:outline-none focus:ring-2 focus:ring-inset focus:ring-yellow-500
        `}
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
  )

  return (
    <>
      {/* <Announcement /> */}
      <div className="sticky top-0 z-50">
        <div className="absolute top-0 h-full w-full "></div>
        <nav className="">
          <div
            className="
              relative mx-auto 
              flex h-16
              justify-between
              lg:container lg:px-16 xl:px-20
            "
          >
            <HamburgerButton toggleFlyOut={() => setOpen(true)} />
            <div
              className="
                flex flex-1
                items-center justify-center
                sm:items-stretch
                lg:justify-between
              "
            >
              <div className="flex items-center justify-start">
                {' '}
                {/* Navbar left side */}
                <div
                  className={`
                  flex-shrink-0 overflow-hidden transition-[max-width,opacity,margin-right] duration-300 ease-in-out 
                  ${
                    showLogo
                      ? 'mr-2 max-w-[200px] opacity-100'
                      : 'mr-0 max-w-0 opacity-0'
                  }
                `}
                >
                  <Logo />
                </div>
                <div className="hidden sm:space-x-1 lg:flex">
                  {/* Docs - Update background */}
                  <Link
                    to="/docs"
                    className="flex items-center rounded-sm bg-neutral-200/70 px-1.5 py-0.5 font-mono text-xs text-neutral-700 backdrop-blur-sm transition-colors duration-150 hover:bg-[#ffcc00] hover:text-neutral-800"
                  >
                    <span>[D] DOCS</span>
                  </Link>
                  {/* Blog - Update background */}
                  <Link
                    to="/blog"
                    className="flex items-center rounded-sm bg-neutral-200/70 p-2 font-mono text-xs text-neutral-700 backdrop-blur-sm transition-colors duration-150 hover:bg-[#ffcc00] hover:text-neutral-800"
                  >
                    <span>[B] BLOG</span>
                  </Link>
                  <GitHubButton />
                  <a
                    href="https://discord.gg/rzdnErX"
                    target="_blank"
                    rel="noreferrer"
                    className="flex items-center rounded-sm bg-neutral-200/70 p-2 font-mono text-xs text-neutral-700 backdrop-blur-sm transition-colors duration-150 hover:bg-[#ffcc00] hover:text-neutral-800"
                  >
                    <span>[D] DISCORD</span>
                  </a>
                  {/* Twitter */}
                  {/* <a
                    href="https://twitter.com/WaspLang"
                    target="_blank"
                    rel="noreferrer"
                    className="flex items-center rounded-sm bg-neutral-200/70 p-2 font-mono text-xs text-neutral-700 backdrop-blur-sm transition-colors duration-150 hover:bg-[#ffcc00] hover:text-neutral-800"
                  >
                    <span>[T] TWITTER</span>
                  </a> */}
                </div>{' '}
                {/* EOF left items */}
              </div>{' '}
              {/* EOF left side */}
              <div className="hidden items-center space-x-1 lg:flex">
                {/* <span className="flex items-center rounded-sm bg-neutral-200/70 p-2 font-mono text-xs text-neutral-700 backdrop-blur-sm transition-colors duration-150 hover:bg-[#ffcc00] hover:text-neutral-800">
                  [C] CODE EXAMPLES
                </span> */}
              </div>{' '}
              {/* EOF right side */}
            </div>
          </div>

          {/* Mobile Nav Menu */}
          <Transition
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
              className={`
                fixed -inset-y-0 z-50 h-screen w-screen transform overflow-y-scroll bg-white p-4 md:p-8
              `}
            >
              <div className="absolute right-4 top-4 items-center justify-between">
                <div className="-mr-2">
                  <button
                    type="button"
                    onClick={() => setOpen(false)}
                    className={`
                      inline-flex items-center justify-center rounded-md bg-white
                      p-2 text-neutral-700 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset
                    `}
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
                {/* ... Mobile menu links (Docs, Blog, GitHub, Discord, Twitter etc.) ... */}
              </div>
            </div>
          </Transition>
        </nav>
      </div>
    </>
  )
}

export default Nav