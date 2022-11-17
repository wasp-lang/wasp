import Image from 'next/image'
import Link from 'next/link'

import SectionContainer from './Layouts/SectionContainer'

const docs = [
  {
    text: 'Getting Started',
    url: '/'
  },
  {
    text: 'Todo app tutorial',
    url: '/'
  },
  {
    text: 'Language reference',
    url: '/'
  }
]

const community = [
  {
    text: 'Discord',
    url: '/'
  },
  {
    text: 'Twitter',
    url: '/'
  },
  {
    text: 'GitHub',
    url: '/'
  }
]

const company = [
  {
    text: 'Blog',
    url: '/'
  },
  {
    text: 'Careers',
    url: '/'
  },
  {
    text: 'Company',
    url: '/'
  }
]

// TODO(matija): duplication, I already have Logo in Nav/index.js
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
    <span className='ml-3 font-semibold text-lg text-neutral-700'>
      Wasp
    </span>
  </div>
)

const Segment = ({ title, links }) => (
  <div>
    <h6 className='text-neutral-700'>{title}</h6>
    <ul className='mt-4 space-y-2'>
      {links.map((l, idx) => {
        return (
          <li>
            <a href={l.url} className='text-sm text-neutral-500'>
              {l.text}
            </a>
          </li>
        )
      })}
    </ul>
  </div>
)

const Footer = () => {
  
  return (
    <footer className='border-t'>
      <SectionContainer>
        <div className='grid grid-cols-1 gap-8 xl:grid xl:grid-cols-3'>

          {/* cols with links */}
          <div className='grid grid-cols-1 xl:col-span-2'>
            <div className='grid grid-cols-2 gap-8 md:grid-cols-3'>

              <Segment title='Docs' links={docs} />
              <Segment title='Community' links={community} />
              <Segment title='Company' links={company} />

            </div>

          </div>

          {/* newsletter part */}
          <div className='xl:col-span-1'>
            <h3 className='text-base'>Stay up to date</h3>
            <p className='mt-4 text-sm text-neutral-500'>
              Join our mailing list and be the first to know when
              we ship new features and updates!
            </p>

            <form className='mt-4 sm:flex sm:max-w-md'>
              <label for='email-address' class='sr-only'>
                Email address
              </label>
              <input
                type="email" name="email-address" id="email-address"
                required autocomplete='email'
                placeholder='you@awesomedev.com'
                className={`
                  text-sm w-full
                  appearance-none
                  placeholder:text-neutral-400
                  border border-yellow-500
                  px-4 py-2 rounded-md bg-transparent
                `}
              />
              <div className='rounded-md mt-3 sm:mt-0 sm:ml-3'>
                <button type='submit'
                  className={`
                    w-full
                    text-sm border border-transparent rounded-md
                    bg-yellow-500 text-white
                    px-4 py-2
                  `}
                >
                  Subscribe
                </button>
              </div>
            </form>

          </div>
        </div>
        <div className='pt-8 mt-8'>
          <Logo />
          <p className='mt-4 text-xs text-neutral-400'>
            © Wasp, Inc. All rights reserved.
          </p>
        </div>
      </SectionContainer>
    </footer>
  )
}

export default Footer
