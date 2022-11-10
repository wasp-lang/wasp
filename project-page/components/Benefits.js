import { Terminal } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

const Lang = () => (
  <>
    <span className='underline decoration-yellow-500 font-bold'>
      language
    </span>;
  </>
)

const Benefits = () => {
  return (
    <SectionContainer className='space-y-16'>
      <div className='grid grid-cols-12'>
        <div className='col-span-12 text-center'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
            Yet another web framework {'=>'} Except it is
            a <Lang />
          </h2>
          <p className='text-neutral-500'>
            Don't worry, it takes less than 30 minutes to learn.
          </p>
        </div>
      </div>

      <dl className='grid grid-cols-1 lg:grid-cols-4'>
        <div className='mb-10 md:mb-0'>
          <div className='flex items-center'>
            <Terminal />
            <dt className=''>
              Benefit name
            </dt>
          </div>

          <p className=''>This is benefit description.</p>

        </div>
        
      </dl>

    </SectionContainer>

  )
}

export default Benefits
