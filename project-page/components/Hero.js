import Link from 'next/link'
import SyntaxHighlighter from 'react-syntax-highlighter'
import { qtcreatorLight, atomOneLight, atomOneDark, a11ylight } from 'react-syntax-highlighter/dist/cjs/styles/hljs'
import { Terminal } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

const StartIcon = () => (
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="16" height="16" viewBox="0 0 24 24" fill="none"
    stroke="currentColor" stroke-width="2" stroke-linecap="round"
    stroke-linejoin="round" opacity="0.5"
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

const codeStringJs = 'const a = 27'

  return (
    <SectionContainer className='pt-24'>
      <div className='lg:grid lg:grid-cols-12 lg:gap-16'>

        <div className='lg:col-span-6 space-y-12'>
          {/* Hero title and subtitle */}
          <div>
            <h1 className='text-4xl lg:text-5xl lg:leading-tight font-extrabold text-neutral-700'>
              Develop full-stack web apps&nbsp;
              <span className='underline decoration-yellow-500'>without boilerplate</span>.
            </h1>

            <p className='mt-4 sm:mt-5 text-xl lg:text-xl text-neutral-500'>
              Describe common features via Wasp DSL and write the rest in React, Node.js
              and Prisma.
            </p>
          </div> {/* EOF Hero title and subtitle */}

          <ActionButtons />

        </div>

        <div className='lg:col-span-6 text-sm mt-16 lg:mt-0 shadow-2xl rounded-md'>
          <SyntaxHighlighter
            language="javascript" 
            style={atomOneLight}
            customStyle={{
              borderRadius: '10px',
              paddingLeft: '15px',
            }}
          >
            {codeString}
          </SyntaxHighlighter>
        </div>

      </div>
    </SectionContainer>
  )

}

export default Hero
