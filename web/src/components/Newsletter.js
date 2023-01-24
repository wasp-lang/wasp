import React from 'react'

import SubscribeForm from './SubscribeForm'
import SectionContainer from './Layouts/SectionContainer'

const Newsletter = () => {
  return (
    <SectionContainer id='signup'>
      <div className='grid grid-cols-12'>
        <div className='col-span-12'>

          <div
            className={`
              px-6 py-6 bg-yellow-500/25 rounded-lg
              md:p-12 lg:p-16
              xl:flex xl:items-center
            `}
          >
            <div className='xl:w-0 xl:flex-1'>
              <h2 className='text-2xl font-extrabold text-neutral-700'>
                Stay up to date 📬
              </h2>
              <p className='mt-3 text-lg text-neutral-500 leading-6'>
                Be the first to know when we ship new features and updates!
              </p>
            </div>

            <div className='mt-8 sm:w-full sm:max-w-md xl:mt-0 xl:ml-8'>
              <SubscribeForm
                className='sm:flex'
                inputBgColor='bg-[#f5f5f5]'
              />

            </div>

          </div>

        </div>
      </div>

    </SectionContainer>
  )
}

export default Newsletter

