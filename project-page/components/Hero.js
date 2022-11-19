import Link from 'next/link'
import SyntaxHighlighter from 'react-syntax-highlighter'
import { qtcreatorLight, atomOneLight, atomOneDark, a11ylight } from 'react-syntax-highlighter/dist/cjs/styles/hljs'
import { Terminal } from 'react-feather'

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
    <Link href='/'>
      <a>
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
          <span>Get started in 5 minutes</span>
          <StartIcon />
        </button>
      </a>
    </Link>

    <Link href='/'>
      <a>
        <button
          className={`
            inline-flex items-center space-x-2
            px-3 py-2 rounded
            border border-neutral-500
            text-sm leading-4
            hover:text-neutral-400 hover:border-neutral-400
            transition ease-out duration-200
          `}
        >
          <Terminal size={16} />
          <span>Showcases</span>
        </button>
      </a>
    </Link>
  </div>
)

const Hero = () => {
  const codeString =
`app todoApp {
  title: "ToDo App", /* visible in tab */

  auth: { /* full-stack auth out-of-the-box */
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      usernameAndPassword: {},
      google: {}
    }
  }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  /* import your React code */
  component: import Main from "@ext/Main.js"
}`

  return (
    <SectionContainer className='pb-5 pt-24'>
      <div className='lg:grid lg:grid-cols-12 lg:gap-16'>

        <div className='lg:col-span-6 space-y-12'>
          {/* Hero title and subtitle */}
          <div>
            <h1
              className={`
                text-4xl lg:text-5xl lg:leading-tight
                font-extrabold text-neutral-700
                dark:text-yellow-500
              `}
            >
              Develop full-stack web apps&nbsp;
              <span className='underline decoration-yellow-500'>without boilerplate</span>.
            </h1>

            <p className='mt-4 sm:mt-5 text-xl lg:text-xl text-neutral-500'>
              Describe common features via Wasp DSL and write the rest in React, Node.js
              and Prisma.
            </p>
          </div> {/* EOF Hero title and subtitle */}

          <ActionButtons />

          <div className='flex flex-col gap-4'>
            <small className='text-neutral-500 text-xs'>works with</small>

            <div className='flex'>
              <img
                className='h-8 md:h-10 pr-5 md:pr-10'
                src='/react-logo-gray.svg'
                alt='React'
              />
              <img
                className='h-8 md:h-10 pr-5 md:pr-10'
                src='/nodejs-logo-gray.svg'
                alt='Node'
              />
              <img
                className='h-8 md:h-10 pr-5 md:pr-10'
                src='/prisma-logo-gray.svg'
                alt='Prisma'
              />
            </div>
          </div>

        </div>

        <div className='lg:col-span-6 lg:mt-0 mt-16'>
          <div className='relative flex flex-col items-center justify-center'>

            <div className='bg-yellow-500/10 flex h-6 w-full items-center justify-between rounded-t-md px-2'>
              <span className='text-sm text-neutral-500'>todoApp.wasp</span>
              <div className='flex'>
                <div className='bg-yellow-500 mr-2 h-2 w-2 rounded-full' />
                <div className='bg-yellow-500 mr-2 h-2 w-2 rounded-full' />
                <div className='bg-yellow-500 mr-2 h-2 w-2 rounded-full' />
              </div>

            </div>

            <div className='w-full text-sm shadow-2xl rounded-b-md'>
              <SyntaxHighlighter
                language="javascript" 
                style={atomOneLight}
                customStyle={{
                  borderBottomLeftRadius: '10px',
                  borderBottomRightRadius: '10px',
                  paddingLeft: '15px',
                }}
              >
                {codeString}
              </SyntaxHighlighter>
            </div> {/* EOF code block wrapper */}

          </div> {/* EOF wrapper of header + code */}

        </div> {/* EOF col-span-6 */}

      </div>
    </SectionContainer>
  )

}

export default Hero
