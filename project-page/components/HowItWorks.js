import SectionContainer from './Layouts/SectionContainer'

const HowItWorks = () => {
  return (
    <SectionContainer>
      <div className='grid grid-cols-12'>
        <div className='col-span-12 lg:col-span-5'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
            How it works?
          </h2>
          <p className='text-neutral-500'>
            Given <code>.wasp</code> and <code>.js(x)/.css/...</code>, source files,
            Wasp compiler generates the full source of your web app in
            the target stack - front-end, back-end and deployment.
          </p>
        </div>
      </div>
    </SectionContainer>
    

  )

}

export default HowItWorks
