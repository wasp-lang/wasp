import React from 'react'
import { ChevronRight, X } from 'react-feather'

const Announcement = () => {

  const handleLink = () => {
    window.open('https://www.producthunt.com/posts/wasp-lang-beta')
  }

  return (
    <div
      onClick={handleLink}
      className={`
        overflow-hidden
        cursor-pointer flex-row
        space-x-3
        text-white
        bg-[#ff6154]
      `}
    >
      <div
        className={`
          mx-auto flex items-center justify-center divide-white p-3
          text-sm font-medium
          lg:container lg:divide-x lg:px-16 xl:px-20
        `}
      >
        <span className='item-center flex gap-2 px-3'>

          <span>Wasp Beta is live on Product Hunt 🚀</span>
        </span>

        <span className='hidden items-center space-x-2 px-3 lg:flex'>
          <span>Support us now!</span>
          <ChevronRight size={14} />
        </span>

      </div>


    </div>

  )

}

export default Announcement
