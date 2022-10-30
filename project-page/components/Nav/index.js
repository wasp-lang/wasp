import Link from 'next/link'
import Image from 'next/image'
import { Star } from 'react-feather'

const Nav = () => {

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
            {/* TODO: HamburgerButton goes here */}
            <div className='
                flex flex-1
                items-center justify-center
                sm:items-stretch
                lg:justify-between
              '
            >
              <div className='flex items-center'> {/* Navbar left side */}
                <div className='flex flex-shrink-0 items-center'> {/* Logo */}
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
                </div> {/* EOF logo */}

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
                {/* GitHub button */}
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
                </a>{/* EOF GitHub button */}
                
              </div> {/* EOF right side */}

            </div>

          </div>
        </nav>


      </div>
    </>
  )

}

export default Nav
