import React from 'react'

import SubscribeForm from './SubscribeForm'
import SectionContainer from './Layouts/SectionContainer'

const Newsletter = () => {
  return (
    <SectionContainer id="signup">
      <div className="grid grid-cols-12">
        <div className="col-span-12">
          <div
            className={`
              rounded-lg bg-yellow-500/25 px-6 py-6
              md:p-12 lg:p-16
              xl:flex xl:items-center
            `}
          >
            <div className="xl:w-0 xl:flex-1">
              <h2 className="text-2xl font-extrabold text-neutral-700">
                Stay up to date 📬
              </h2>
              <p className="mt-3 text-lg leading-6 text-neutral-500">
                Be the first to know when we ship new features and updates!
              </p>
            </div>

            <div className="mt-8 sm:w-full sm:max-w-md xl:ml-8 xl:mt-0">
              <SubscribeForm inputBgColor="bg-[#f5f5f5]" />
            </div>
          </div>
        </div>
      </div>
    </SectionContainer>
  )
}

export default Newsletter
