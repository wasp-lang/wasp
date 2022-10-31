import Link from 'next/link'
import Image from 'next/image'
import { Star, Twitter } from 'react-feather'

import { DiscordIcon } from './SocialIcons'

const Nav = () => {

  const Logo = () => (
    <div className='flex flex-shrink-0 items-center'>
      <Link href='/' as='/'>
        <a>
          <Image
            src='/wasp-logo.png'
            width={35}
            height={35}
            alt='Wasp Logo'
          />
        </a>
      </Link>
      <span className='ml-3 font-semibold text-lg'>
        Wasp <sup className='text-base text-yellow-500'>βeta</sup>
      </span>
    </div>
  )

  const GitHubButton = () => (
    <a href='/'
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
        <nav className='border-b'>
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
                  <Link href='/docs'>
                    <a 
                      className={`
                        p-5 px-1
                        hover:text-yellow-500 hover:border-yellow-500
                        border-b-2 border-transparent
                        text-sm font-medium
                      `}
                    >
                      Docs
                    </a>
                  </Link>
                  <Link href='/blog'>
                    <a 
                      className={`
                        p-5 px-1
                        hover:text-yellow-500 hover:border-yellow-500
                        border-b-2 border-transparent
                        text-sm font-medium
                      `}
                    >
                      Blog
                    </a>
                  </Link>
                  
                </div> {/* EOF left items */}
              </div> {/* EOF left side */}

              <div className='flex items-center gap-2'> {/* Navbar right side */}
                <GitHubButton />
                <a href='' className='hidden lg:flex'>
                  <DiscordIcon />
                </a>
              </div> {/* EOF right side */}

            </div>

          </div>
        </nav>


      </div>
    </>
  )

}

export default Nav
