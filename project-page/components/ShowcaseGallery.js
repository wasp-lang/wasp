import Image from 'next/image'
import SectionContainer from './Layouts/SectionContainer'

const Tag = ({ text, color }) => (
  <span className={`text-sm ${color}`}>
    {text}
  </span>
)

const ShowcaseItem = ({ url, thumb, title, description }) => (
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
            <Tag text='hackathon' color='text-yellow-500' />
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
          description='Check out how we made this first showcase post.'
        />

        <ShowcaseItem
          url='/'
          thumb='/wasp-compilation-diagram.png'
          title='Second showcase post'
          description='Check out how we made this second showcase post.'
        />

        <ShowcaseItem
          url='/'
          thumb='/wasp-compilation-diagram.png'
          title='Third showcase post'
          description='Check out how we made this third showcase post.'
        />

      </div>


    </SectionContainer>
  )
}

export default ShowcaseGallery
