import SectionContainer from './Layouts/SectionContainer'

const Feature = ({ title, description }) => (
  <div className='col-span-12 md:col-span-6'>
    <div className='lg:mt-5'>
      <dt>
        <h4 className='mb-4'>{ title }</h4>
        <p className='text-neutral-500'>
          { description }
        </p>
        <a href='/' className='mt-3 block cursor-pointer text-sm text-neutral-500'>
          <span>Learn more</span>
        </a>
      </dt>
    </div>
  </div>

)

const HowItWorks = () => {
  return (
    <SectionContainer>
      <div className='grid grid-cols-12'>

        <div className='col-span-12 lg:col-span-4'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
             What's under the hood? 🚕
          </h2>
          <p className='text-neutral-500'>
            Given <code>.wasp</code> and <code>.js(x)/.css/...</code>, source files,
            Wasp compiler generates the full source of your web app in
            the target stack - front-end, back-end and deployment.
          </p>

          {/* Features */}
          <div className='py-8'>
            <dl className='grid grid-cols-12 gap-y-4 md:gap-8'>

              <Feature 
                title='Typescript support'
                description="JS or TS - mix'n'match as you wish."
              />

              <Feature 
                title='Wasp CLI'
                description='All handy commands at the tip of your fingers.'
              />

              <Feature 
                title='LSP for VS Code'
                description='Syntax highligthing, go-to-definition, etc. work out-of-the-box.'
              />

              <Feature 
                title='Deploy anywhere'
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
