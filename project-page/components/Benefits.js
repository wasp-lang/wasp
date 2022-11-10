import { Terminal } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

const Lang = () => (
  <>
    <span className='underline decoration-yellow-500 font-bold'>
      language
    </span>
  </>
)

const Benefit = ({ Icon, title, description }) => (
  <div className='mb-10 md:mb-0 space-y-4'>
    <div className='flex items-center'>
      <div
        className={`
          inline-flex h-8 w-8 rounded-md
          items-center justify-center
          text-yellow-500 bg-neutral-700
        `}
      >
        <Icon size={20} />
      </div>
      <dt className='ml-4'>
        { title }
      </dt>
    </div>
    <p className=''>
      { description }
    </p>
  </div>
)

const Benefits = () => {
  return (
    <SectionContainer className='space-y-16'>
      <div className='grid grid-cols-12'>
        <div className='col-span-12 text-center'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
            Yet another web framework. Except it is
            a <Lang />.
          </h2>
          <p className='text-neutral-500'>
            Don't worry, it takes less than 30 minutes to learn.
          </p>
        </div>
      </div>

      <dl className='grid grid-cols-1 lg:grid-cols-4 md:gap-16'>

        <Benefit
          Icon={Terminal}
          title='Some benefit'
          description={`
            This benefit is really cool. This is the sentence that describes it
            really well and has multiple lines.
          `}
        />

        <Benefit
          Icon={Terminal}
          title='Some benefit'
          description={`
            This benefit is really cool. This is the sentence that describes it
            really well and has multiple lines.
          `}
        />

        <Benefit
          Icon={Terminal}
          title='Some benefit'
          description={`
            This benefit is really cool. This is the sentence that describes it
            really well and has multiple lines.
          `}
        />

        <Benefit
          Icon={Terminal}
          title='Some benefit'
          description={`
            This benefit is really cool. This is the sentence that describes it
            really well and has multiple lines.
          `}
        />
        
      </dl>

    </SectionContainer>

  )
}

export default Benefits
