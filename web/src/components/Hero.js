import React from 'react'
import Link from '@docusaurus/Link'

import './prismCustomization'

import CodeHighlight from './CodeHighlight'

import { Terminal, ArrowUpRight, Play, BookOpen, Grid, Layout, Trello   } from 'react-feather'

// Terminal, BookOpen, Grid, Layout, Trello, FileText 

import InstallCmd from './InstallCmd'
import SectionContainer from './Layouts/SectionContainer'

const StartIcon = () => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="16" height="16" viewBox="0 0 24 24" fill="none"
    stroke="currentColor" strokeWidth="2" strokeLinecap="round"
    strokeLinejoin="round" opacity="0.5"
  >
    <polyline points="13 17 18 12 13 7"></polyline>
    <polyline points="6 17 11 12 6 7"></polyline>
  </svg>
)

const ActionButtons = () => (
  <div className='flex items-center gap-2'>
    <Link to='/docs/quick-start'>
      <button
        className={`
          inline-flex items-center space-x-2
          px-3 py-2 rounded
          bg-yellow-500 text-white text-sm leading-4
          border border-yellow-500 hover:border-yellow-400
          hover:bg-yellow-400
          transition ease-out duration-200
        `}
      >
        <Terminal size={16} />
        <span>{'wasp new <app>'}</span>
      </button>
    </Link>

    <Link to='/docs'>
      <button
        className={`
          inline-flex items-center space-x-2
          px-3 py-2 rounded
          border border-neutral-500
          text-sm leading-4
          text-neutral-700
          hover:text-neutral-400 hover:border-neutral-400
          transition ease-out duration-200
        `}
      >
        <BookOpen size={16} />
        <span>Quick Guide</span>
      </button>
    </Link>
  </div>
)

const PHBadge = () => (
  <a
    href="https://www.producthunt.com/posts/wasp-lang-beta"
    target="_blank"
    rel="noreferrer"
  >
    <img
      className='w-32 md:w-[180px]'
      src="https://api.producthunt.com/widgets/embed-image/v1/top-post-badge.svg?post_id=277135&theme=light&period=daily"
      alt="Wasp&#0045;lang&#0032;Alpha - Develop&#0032;web&#0032;apps&#0032;in&#0032;React&#0032;&#0038;&#0032;Node&#0046;js&#0032;with&#0032;no&#0032;boilerplate | Product Hunt"
    />
  </a>
)

const Hero = () => {
  const codeString =
`app todoApp {
  title: "ToDo App",  // visible in the browser tab
  auth: { // full-stack auth out-of-the-box
    userEntity: User, 
    methods: { google: {}, gitHub: {}, email: {...} }
  }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  authRequired: true, // Limit access to logged in users.
  component: import Main from "@client/Main.tsx" // Your React code.
}

query getTasks {
  fn: import { getTasks } from "@server/tasks.js", // Your Node.js code.
  entities: [Task] // Automatic cache invalidation.
}

entity Task {=psl ... psl=} // Your Prisma data model.
`

  return (
    <SectionContainer className='pb-5 pt-24'>
      <div className='lg:grid lg:grid-cols-12 lg:gap-16'>

        <div className='lg:col-span-6 space-y-12 z-10'>
          {/* Hero title and subtitle */}
          <div>
            <h1
              className={`
                text-4xl lg:text-5xl lg:leading-tight
                font-extrabold text-neutral-700
              `}
            >
              Develop full-stack web apps <span className='underline decoration-yellow-500'>faster</span>.
            </h1>

            <p className='mt-4 sm:mt-5 text-xl lg:text-xl text-neutral-500'>
              Rails-like framework for React, Node.js and Prisma. Build your app in a day and deploy it with a single CLI command.
            </p>
          </div> {/* EOF Hero title and subtitle */}

          <ActionButtons />

          <div className='flex flex-col gap-4'>
            <small className='text-neutral-500 text-xs'>Works with</small>

            <div className='flex'>
              <img
                className='h-8 md:h-10 pr-5 md:pr-10'
                src='img/lp/react-logo-gray.svg'
                alt='React'
              />
              <img
                className='h-8 md:h-10 pr-5 md:pr-10'
                src='img/lp/nodejs-logo-gray.svg'
                alt='Node'
              />
              <img
                className='h-8 md:h-10 pr-5 md:pr-10'
                src='img/lp/prisma-logo-gray.svg'
                alt='Prisma'
              />
            </div>

            <span className='flex items-center mt-6'>
              <small className='text-neutral-500 text-xs'>Backed by</small>
              <img
                className='w-24 ml-2'
                src='img/lp/yc-logo-rounded.png'
                alt='YC'
              />
            </span>
          </div>

        </div>

        <div className='lg:col-span-6 lg:mt-0 mt-16'>
          <div className='relative flex flex-col items-center justify-center'>
            {/* Editor header bar */}
            <div className='bg-yellow-500/10 flex h-6 w-full items-center justify-between rounded-t-md px-2'>
              <Link to='https://github.com/wasp-lang/wasp/blob/main/examples/todo-typescript/main.wasp'>
                <span
                  className={`
                    text-sm text-neutral-500 flex items-center space-x-1 hover:text-neutral-400
                    transition ease-out duration-200
                  `}
                >
                  <span>todoApp.wasp</span>
                  <ArrowUpRight size={14} />
                  <span className='text-neutral-400'>· Wasp config file</span>
                </span>
              </Link>
              <div className='flex space-x-2'>
                <div className='bg-yellow-500 h-2 w-2 rounded-full' />
                <div className='bg-yellow-500 h-2 w-2 rounded-full' />
                <div className='bg-yellow-500 h-2 w-2 rounded-full' />
              </div>
            </div>
            {/* Editor body */}
            <div className='w-full text-sm shadow-2xl rounded-b-md'>
              <CodeHighlight
                language='wasp'
                source={codeString}
              />
            </div> {/* EOF code block wrapper */}
          </div> {/* EOF wrapper of header + code */}
        </div> {/* EOF col-span-6 */}

      </div>


      {/* 1-min video */}
      {/*
      <div className='flex justify-center mt-20'>
        <div className='w-full lg:w-2/3 xl:w-3/5'>
          <div
            className="relative w-full rounded-md shadow-lg"
            style={{ padding: '56.25% 0 0 0' }}
          >
            <iframe
              title="Demo video showcasing Wasp"
              className="absolute h-full w-full rounded-md"
              src="https://www.youtube-nocookie.com/embed/YaaTJOhx68I?playlist=YaaTJOhx68I&autoplay=0&loop=1&controls=0&showinfo=1&modestbranding=0&rel=0&disablekb=0&mute=1"
              style={{ top: 0, left: 0 }}
              frameBorder="0"
              allow="autoplay; modestbranding; encrypted-media"
            />
          </div>
        </div>
      </div>
      */}

      {/* PH & YC badges */}
      {/*
      <div className='flex justify-center items-center space-x-4 mt-20 mb-10 md:mt-28 md:mb-0'>
        <PHBadge />
        <div
          className={`
            h-11 border border-transparent border-l-neutral-400/50
          `}
        />
        <img
          className='w-32 md:w-[180px]'
          src='img/lp/yc-logo.png'
          alt='YC'
        />
      </div>
      */}

    </SectionContainer>
  )

}

export default Hero
