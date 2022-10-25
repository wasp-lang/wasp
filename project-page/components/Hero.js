import SectionContainer from './Layouts/SectionContainer'

import SyntaxHighlighter from 'react-syntax-highlighter'
import { docco, gruvboxDark, dark, monokai, atomOneDark, stackoverflowDark, androidstudio, dracula, darcula }
  from 'react-syntax-highlighter/dist/esm/styles/hljs'


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
          <h1 className='text-4xl'>
            Develop full-stack web apps with less code.
          </h1>

          <p className='mt-1.5 sm:mt-5 text-lg'>
            Describe common features in Wasp DSL and write the rest in React, Node.js and Prisma.
          </p>
        </div>

        <div className='lg:col-span-6 text-sm mt-16 lg:mt-0'>
          <SyntaxHighlighter
            language="javascript" 
            style={atomOneDark}
            showLineNumbers
            customStyle={{
              borderRadius: '5px',
              paddingLeft: '15px'
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
