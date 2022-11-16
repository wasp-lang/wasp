import classNames from 'classnames'
import Image from 'next/image'
import SectionContainer from './Layouts/SectionContainer'

const Tag = ({ text, className }) => (
  <span
    className={classNames(`
      text-sm border rounded-md px-2.5 py-0.5

    `, className
    )}
  >
    {text}
  </span>
)

const ShowcaseItem = ({ url, thumb, title, description, children }) => (
  <div>
    <a href={url}>
      <div className='group inline-block min-w-full'>
        <div className='flex flex-col space-y-3 pb-8'>

          <div
            className={`
              border-neutral-300 relative mb-4 h-60 w-full overflow-auto
              rounded-lg border shadow-lg
            `}
          >
            <Image 
              layout='fill'
              src={thumb}
              objectFit='cover'
              className=''
            />
          </div>

          <h3 className='text-xl'>{title}</h3>

          <div className='flex space-x-2'>
            {children}
          </div>

          <p className='text-base'>{description}</p>

        </div>
      </div>
    </a>
  </div>
  
)

const ShowcaseGallery = () => {
  return (
    <SectionContainer className='space-y-16'>
      <div className='grid grid-cols-12'>
        <div className='col-span-12 text-center'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
            🏆 Showcase Gallery 🏆
          </h2>
          <p className='text-neutral-500'>
            See what others are building with Wasp.
          </p>
        </div>
      </div>

      <div
        className={`
          mx-auto grid max-w-lg gap-8
          lg:max-w-none lg:grid-cols-3 lg:gap-12
        `}
      >
        <ShowcaseItem
          url='/'
          thumb='/showcase/farnance-dashboard.png'
          title='Farnance: SaaS marketplace for farmers'
          description="See how Julian won HackLBS 2021 among 250 participants and why Wasp was instrumental for the team's victory."
        >
          <Tag text='hackathon' className='text-yellow-600 border-yellow-600 bg-yellow-50' />
          <Tag text='material-ui' className='text-blue-500 border-blue-500 bg-slate-50' />
        </ShowcaseItem>

        <ShowcaseItem
          url='/'
          thumb='/showcase/grabbit-hero.png'
          title='Grabbit: Easily manage dev environments'
          description='See how Michael built and deployed an internal tool for managing dev resources at StudentBeans.'
        >
          <Tag text='internal-tools' className='text-green-600 border-green-600 bg-green-50' />
        </ShowcaseItem>

        <ShowcaseItem
          url='/'
          thumb='/showcase/amicus-landing.png'
          title='Amicus: Task and workflow management for legal teams'
          description='See how Erlis rolled out fully-fledged SaaS as a team of one in record time and got first paying customers.'
        >
          <Tag text='startup' className='text-fuchsia-600 border-fuchsia-600 bg-fuchsia-50' />
          <Tag text='material-ui' className='text-blue-500 border-blue-500 bg-slate-50' />
        </ShowcaseItem>

      </div>


    </SectionContainer>
  )
}

export default ShowcaseGallery
