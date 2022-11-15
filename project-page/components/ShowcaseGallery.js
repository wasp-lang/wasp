import SectionContainer from './Layouts/SectionContainer'

const ShowcaseGallery = () => {
  return (
    <SectionContainer className=''>
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


    </SectionContainer>
  )
}

export default ShowcaseGallery
