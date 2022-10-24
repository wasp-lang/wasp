import React from 'react'
import Link from '@docusaurus/Link'
import classNames from 'classnames'

import SectionContainer from './Layouts/SectionContainer'
import SubscribeForm from './SubscribeForm'
import DarkModeToggle from './DarkModeToggle'

const docs = [
  {
    text: 'Getting Started',
    url: '/docs'
  },
  {
    text: 'Todo app tutorial',
    url: '/docs/tutorials/todo-app'
  },
  {
    text: 'Language reference',
    url: '/docs/language/overview'
  }
]

const community = [
  {
    text: 'Discord',
    url: 'https://discord.gg/rzdnErX'
  },
  {
    text: 'Twitter',
    url: 'https://twitter.com/WaspLang'
  },
  {
    text: 'GitHub',
    url: 'https://github.com/wasp-lang/wasp'
  }
]

const company = [
  {
    text: 'Blog',
    url: '/blog'
  },
  {
    text: 'Careers',
    url: 'https://www.notion.so/wasp-lang/Founding-Engineer-at-Wasp-88a73838f7f04ab3aee1f8e1c1bee6dd'
  },
  {
    text: 'Company',
    url: 'https://www.notion.so/wasp-lang/Founding-Engineer-at-Wasp-88a73838f7f04ab3aee1f8e1c1bee6dd#20569f14a8af452db10ae618d764d505'
  }
]

// TODO(matija): duplication, I already have Logo in Nav/index.js
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
          <li key={idx}>
            <a
              href={l.url}
              className={`
                text-sm text-neutral-500 hover:text-neutral-400
                transition-colors
              `}
            >
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

            <SubscribeForm
              className='mt-4 sm:flex sm:max-w-md'
              inputBgColor='bg-transparent'
            />

          </div>
        </div>
        <div className='pt-8 mt-8'>
          <Logo />
          <div className='flex justify-between'>
            <p className='mt-4 text-xs text-neutral-400'>
              © Wasp, Inc. All rights reserved.
            </p>
            <DarkModeToggle />
          </div>
        </div>
      </SectionContainer>
    </footer>
  )
}

export default Footer
