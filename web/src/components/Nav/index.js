import React from 'react'
import Link from '@docusaurus/Link'
import { Star, Twitter } from 'react-feather'

import { DiscordIcon, TwitterIcon } from './SocialIcons'

const Nav = () => {

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
        hidden lg:flex hover:opacity-75 py-5
        hover:text-yellow-500 hover:border-yellow-500
        border-b-2 border-transparent
      `}
    >
      <Icon />
    </a>
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
          group-hover:text-yellow-500
        `}
      >
        <Star strokeWidth={2} />
      </div>
      <span className='truncate'>Star us on GitHub</span>
    </a>
  )

  const HamburgerButton = () => (
    <div className='absolute inset-y-0 left-0 px-2 flex items-center lg:hidden'>
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
            <HamburgerButton />
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
                      <span className='px-2 py-1 rounded bg-yellow-500/25 hover:bg-yellow-500/10'>
                        📬 Join the list
                      </span>
                    </span>
                  </Link>
                  
                </div> {/* EOF left items */}
              </div> {/* EOF left side */}

              <div className='flex items-center gap-2 space-x-2'> {/* Navbar right side */}
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
                    Get started
                  </button>
                </Link>

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
        </nav>


      </div>
    </>
  )

}

export default Nav
