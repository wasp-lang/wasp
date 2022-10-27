import SectionContainer from './Layouts/SectionContainer'

import SyntaxHighlighter from 'react-syntax-highlighter'
import { qtcreatorLight, atomOneLight, atomOneDark, a11ylight } from 'react-syntax-highlighter/dist/cjs/styles/hljs'


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
        <div className='lg:col-span-6'>
          <h1 className='text-4xl lg:text-5xl font-extrabold text-neutral-700'>
            Develop full-stack web apps <span className='underline'>without boilerplate</span>.
          </h1>

          <p className='mt-1.5 sm:mt-5 text-xl text-neutral-500'>
            Describe common features in Wasp DSL and write the rest in React, Node.js and Prisma.
          </p>
        </div>

        <div className='lg:col-span-6 text-sm mt-16 lg:mt-0 shadow-2xl'>
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
