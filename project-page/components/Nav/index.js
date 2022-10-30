import Link from 'next/link'
import Image from 'next/image'

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
                        text-base font-medium
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
                        text-base font-medium
                      `}
                    >
                      Blog
                    </a>
                  </Link>
                  
                </div> {/* EOF left items */}
              </div> {/* EOF left side */}

              <div className='flex items-center gap-2'> {/* Navbar right side */}
                Right side of navbar
                
              </div> {/* EOF right side */}

            </div>

          </div>
        </nav>


      </div>
    </>
  )

}

export default Nav
