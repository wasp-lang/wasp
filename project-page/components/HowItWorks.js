import SectionContainer from './Layouts/SectionContainer'

const HowItWorks = () => {
  return (
    <SectionContainer>
      <div className='grid grid-cols-12'>

        <div className='col-span-12 lg:col-span-5'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
            Under the hood 🚕
          </h2>
          <p className='text-neutral-500'>
            Given <code>.wasp</code> and <code>.js(x)/.css/...</code>, source files,
            Wasp compiler generates the full source of your web app in
            the target stack - front-end, back-end and deployment.
          </p>

          {/* Features */}
          <div className='py-8'>
            <dl className='grid grid-cols-12 gap-y-4 md:gap-8'>

              <div className='col-span-12 md:col-span-6'>
                <div className='lg:mt-5'>
                  <dt>
                    <h4 className='mb-4'>Typescript support</h4>
                    <p className='text-neutral-500'>
                      This is some description of our cool Typescript support.
                    </p>
                    <a href='/' className='mt-3 block cursor-pointer text-sm text-neutral-500'>
                      <span>Learn more</span>
                    </a>
                  </dt>
                </div>
              </div>

              <div className='col-span-12 md:col-span-6'>
                <div className='lg:mt-5'>
                  <dt>
                    <h4 className='mb-4'>Awesome CLI</h4>
                    <p className='text-neutral-500'>
                      Our CLI is super helpful and you will feel amazing using it.
                    </p>
                    <a href='/' className='mt-3 block cursor-pointer text-sm text-neutral-500'>
                      <span>Learn more</span>
                    </a>
                  </dt>
                </div>
              </div>

              <div className='col-span-12 md:col-span-6'>
                <div className='lg:mt-5'>
                  <dt>
                    <h4 className='mb-4'>Custom made LSP</h4>
                    <p className='text-neutral-500'>
                      Our LSP is here, enjoy it VS Code.
                    </p>
                    <a href='/' className='mt-3 block cursor-pointer text-sm text-neutral-500'>
                      <span>Learn more</span>
                    </a>
                  </dt>
                </div>
              </div>

            </dl>

          </div> {/* EOF Features */}
        </div>

        <div className='col-span-12 lg:col-span-7 xl:col-span-6 xl:col-start-7'>
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
