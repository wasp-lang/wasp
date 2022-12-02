import React from 'react'
import { useHistory } from '@docusaurus/router'
import { ChevronRight, X } from 'react-feather'

const Announcement = () => {
  let history = useHistory()

  const handleLink = () => {
    //history.push('/blog/2022/11/26/wasp-beta-launch-week')

    window.open('https://betathon.wasp-lang.dev/')
  }

  return (
    <div
      onClick={handleLink}
      className={`
        overflow-hidden
        cursor-pointer flex-row
        space-x-3
        text-white
        bg-yellow-500
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

          <span>Beta Launch Week Day 6: <span className='underline'>Hackathon!</span> 🚀</span>
        </span>

        <span className='hidden items-center space-x-2 px-3 lg:flex'>
          <span>See more</span>
          <ChevronRight size={14} />
        </span>

      </div>


    </div>

  )

}

export default Announcement
