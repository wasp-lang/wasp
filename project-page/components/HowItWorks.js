import { ArrowRight } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

const Feature = ({ title, description, url }) => (
  <div className='col-span-12 md:col-span-6'>
    <div className='lg:mt-5'>
      <dt>
        <h4 className='mb-4'>
          <span className='bg-yellow-500/25 px-2 py-1 rounded'>{ title }</span>
        </h4>
        <p className='text-neutral-600'>
          { description }
        </p>
        <TextLink url={url} label='Learn more' />
      </dt>
    </div>
  </div>
)

const TextLink = ({ url, label }) => (
  <a href={url}
    className={`
      mt-3 block cursor-pointer text-sm
      text-neutral-600 hover:text-neutral-500
    `}
  >
    <div className='group flex gap-1 items-center'>
      <span>{label}</span>
      <div className='transition-all group-hover:ml-0.5'>
        <span className='text-yellow-600'>
          <ArrowRight size={14} strokeWidth={2} />
        </span>
      </div>
    </div>
  </a>
)

const HowItWorks = () => {
  return (
    <SectionContainer className='lg:pb-8'>
      <div className='grid grid-cols-12'>

        <div className='col-span-12 lg:col-span-4'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
             What&apos;s under the hood? 🚕
          </h2>
          <p className='text-neutral-700'>
            Given <code>.wasp</code> and <code>.js(x)/.css/...</code>, source files,
            Wasp compiler generates the full source of your web app in
            the target stack - front-end, back-end and deployment.
          </p>

          {/* Features */}
          <div className='py-8'>
            <dl className='grid grid-cols-12 gap-y-4 md:gap-8'>

              <Feature 
                title='Typescript support'
                url='/'
                description="JS or TS - mix'n'match as you wish."
              />

              <Feature 
                title='Wasp CLI'
                url='/'
                description='All the handy commands at your fingertips.'
              />

              <Feature 
                title='LSP for VS Code'
                url='/'
                description='Syntax highligthing, go-to-definition, etc. work out-of-the-box.'
              />

              <Feature 
                title='Deploy anywhere'
                url='/'
                description='See our deployment guide for more details.'
              />

            </dl>

          </div> {/* EOF Features */}
        </div>

        <div className='col-span-12 lg:col-span-7 xl:col-span-7 xl:col-start-6'>
          <img
            className=''
            src='/wasp-compilation-diagram.png'
            alt='React'
          />
        </div>

      </div>
    </SectionContainer>
    

  )

}

export default HowItWorks
