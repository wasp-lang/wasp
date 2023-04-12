import React, { useState } from 'react'
import Link from '@docusaurus/Link'
import { Star, Twitter } from 'react-feather'

import Announcement from './Announcement'
import Transition from '../../lib/Transition'
import { GitHubIcon, DiscordIcon, TwitterIcon } from './SocialIcons'

const Nav = () => {

  const [open, setOpen] = useState(false)

  const Logo = () => (
    <div className='flex flex-shrink-0 items-center'>
      <Link to='/'>
        <img
          src='img/lp/wasp-logo.png'
          width={35}
          height={35}
          alt='Wasp Logo'
        />
      </Link>
      <span className='ml-3 font-semibold text-lg text-neutral-700'>
        Wasp <sup className='text-base text-yellow-500'>βeta</sup>
      </span>
    </div>
  )

  const SocialIcon = ({ Icon, url }) => (
    <a href={url} target='_blank' rel='noreferrer'
      className={`
        text-neutral-700
        hidden lg:flex hover:opacity-75 py-5
        hover:text-yellow-500 hover:border-yellow-500
        border-b-2 border-transparent
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
      >
      </iframe>
    </>
  )

  // NOTE(matija): this one does not show the exact count, but e.g. 2k.
  const WaspGhStarsShield = () => (
    <img alt="GitHub Repo stars" src="https://img.shields.io/github/stars/wasp-lang/wasp?style=social" />
  )

  const GitHubButton = () => (
    <a href='https://github.com/wasp-lang/wasp' target='_blank' rel='noreferrer'
      className={`
        px-2.5 py-1 rounded
        hover:bg-neutral-200
        transition ease-out duration-200
        hidden lg:flex items-center space-x-2 text-xs
        group
      `}
    >
      <div
        className={`
          flex h-3 w-3 items-center justify-center
          group-hover:h-4 group-hover:w-4
          text-neutral-700
          group-hover:text-yellow-500
        `}
      >
        <Star strokeWidth={2} />
      </div>
      <span className='truncate text-neutral-700'>Star us on GitHub</span>
    </a>
  )

  const HamburgerButton = ({ toggleFlyOut }) => (
    <div
      className='absolute inset-y-0 left-0 px-2 flex items-center lg:hidden'
      onClick={() => toggleFlyOut()}
    >
      <button
        className={`
          inline-flex items-center p-2
          rounded-md hover:bg-gray-50
          focus:ring-yellow-500 focus:outline-none focus:ring-2 focus:ring-inset
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
      <Announcement />
      <div className='sticky top-0 z-50'>
        <div className='bg-[#f5f4f0] absolute top-0 h-full w-full opacity-80'></div>
        <nav className='border-b backdrop-blur-sm'>
          <div className='
              relative mx-auto 
              flex justify-between
              h-16
              lg:container lg:px-16 xl:px-20
            '
          >
            <HamburgerButton toggleFlyOut={() => setOpen(true)} />
            <div className='
                flex flex-1
                items-center justify-center
                sm:items-stretch
                lg:justify-between
              '
            >
              <div className='flex items-center'> {/* Navbar left side */}
                <Logo />

                <div className='hidden pl-4 sm:ml-6 sm:space-x-4 lg:flex'> {/* Left items */}
                  {/* Docs */}
                  <Link to='/docs'>
                    <span
                      className={`
                        py-5 px-1
                        text-neutral-700
                        hover:text-yellow-500 hover:border-yellow-500
                        border-b-solid border-b-2 border-transparent
                        text-sm font-semibold
                      `}
                    >
                      Docs
                    </span>
                  </Link>

                  {/* Blog */}
                  <Link to='/blog'>
                    <span
                      className={`
                        py-5 px-1
                        text-neutral-700
                        hover:text-yellow-500 hover:border-yellow-500
                        border-b-2 border-transparent
                        text-sm font-semibold
                      `}
                    >
                      Blog
                    </span>
                  </Link>

                  {/* FAQ */}
                  <Link to='#faq'>
                    <span
                      className={`
                        py-5 px-1
                        text-neutral-700
                        hover:text-yellow-500 hover:border-yellow-500
                        border-b-2 border-transparent
                        text-sm font-medium
                      `}
                    >
                      FAQ
                    </span>
                  </Link>

                  {/* Join newsletter */}
                  <Link to='#signup'>
                    <span
                      className={`
                        py-5 px-1
                        border-b-2 border-transparent
                        text-sm font-medium
                      `}
                    >
                      <span
                        className={`
                          text-neutral-700
                          px-2 py-1 rounded bg-yellow-500/25 hover:bg-yellow-500/10
                        `}
                      >
                        📬 Join the list
                      </span>
                    </span>
                  </Link>

                </div> {/* EOF left items */}
              </div> {/* EOF left side */}

              <div className='flex items-center gap-2 space-x-2'> {/* Navbar right side */}
                {/* GH stars badge */}
                {/*
                <span className='hidden lg:inline'>
                  <Link to='https://github.com/wasp-lang/wasp' className='flex items-center'>
                    <WaspGhStarsExactCount />
                  </Link>
                </span>
                */}

                <GitHubButton />

                <Link to='/docs'>
                  <button
                    className={`
                      hidden lg:block text-xs
                      px-2.5 py-1 rounded
                      bg-yellow-500 text-white
                      hover:bg-yellow-400
                      transition ease-out duration-200
                    `}
                  >
                    {'wasp new <app>'}
                  </button>
                </Link>

                <SocialIcon
                  Icon={ GitHubIcon } 
                  url='https://github.com/wasp-lang/wasp'
                />

                <SocialIcon
                  Icon={ DiscordIcon } 
                  url='https://discord.gg/rzdnErX'
                />

                <SocialIcon
                  Icon={ TwitterIcon } 
                  url='https://twitter.com/WaspLang'
                />
              </div> {/* EOF right side */}
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
              <div className='absolute right-4 top-4 items-center justify-between'>
                <div className="-mr-2">
                  <button
                    type="button"
                    onClick={() => setOpen(false)}
                    className={`
                      text-neutral-700 inline-flex items-center justify-center rounded-md
                      bg-white p-2 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset
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

              <div className='mt-6 mb-12'>
                {/* Docs */}
                <div className='space-y-1 pt-2 pb-4'>
                  <Link to="/docs">
                    <span className="text-neutral-700 block pl-3 pr-4 text-base font-medium">
                      Docs
                    </span>
                  </Link>
                </div>

                {/* Docs */}
                <div className='space-y-1 pt-2 pb-4'>
                  <Link to="/blog">
                    <span className="text-neutral-700 block pl-3 pr-4 text-base font-medium">
                      Blog
                    </span>
                  </Link>
                </div>

                {/* FAQ */}
                <div className='space-y-1 pt-2 pb-4'>
                  <Link to="#faq" onClick={() => setOpen(false)}>
                    <span className="text-neutral-700 block pl-3 pr-4 text-base font-medium">
                      FAQ
                    </span>
                  </Link>
                </div>

                {/* Join the list */}
                <div className='space-y-1 pt-2 pb-4'>
                  <Link to="#signup" onClick={() => setOpen(false)}>
                    <span
                      className={`
                        text-neutral-700 block pl-3 pr-4 text-base font-medium
                        px-2 py-1 rounded bg-yellow-500/25 hover:bg-yellow-500/10
                    `}>
                      📬 Join the list
                    </span>
                  </Link>
                </div>

                {/* GitHub */}
                <div className='space-y-1 pt-2 pb-4'>
                  <Link to="https://github.com/wasp-lang/wasp">
                    <span className='flex items-center'>
                      <span className="text-neutral-700 pl-3 pr-4 text-base font-medium">
                        ⭐️ GitHub
                      </span>
                      <WaspGhStarsExactCount />
                    </span>
                  </Link>
                </div>

                {/* Discord */}
                <div className='space-y-1 pt-2 pb-4'>
                  <Link to="https://discord.gg/rzdnErX">
                    <span className="text-neutral-700 block pl-3 pr-4 text-base font-medium">
                      👾 Discord
                    </span>
                  </Link>
                </div>

                {/* Twitter */}
                <div className='space-y-1 pt-2 pb-4'>
                  <Link to="https://twitter.com/WaspLang">
                    <span className="text-neutral-700 block pl-3 pr-4 text-base font-medium">
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
  )

}

export default Nav
