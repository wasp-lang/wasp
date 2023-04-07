import React from 'react'
import { useHistory } from '@docusaurus/router'
import { ChevronRight, X } from 'react-feather'

const Announcement = () => {
  let history = useHistory()

  const handleLink = () => {
    //history.push('/blog/2022/11/26/wasp-beta-launch-week')
    history.push('/#signup')

    //window.open('https://betathon.wasp-lang.dev/')
  }

  return (
    <div
      onClick={handleLink}
      className={`
        overflow-hidden
        cursor-pointer flex-row
        space-x-3
        text-white

        bg-gradient-to-r from-fuchsia-600 to-[#ffcc00]
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

          <span>Wasp Launch Week 2 starts on <span className='underline'>April 12</span>! 🚀</span>
        </span>

        <span className='hidden items-center space-x-2 px-3 lg:flex'>
          <span
            className={`
              bg-neutral-700 px-2.5 py-1 text-xs rounded-full cursor-pointer
              hover:bg-neutral-600
            `}
          >
            Notify me
          </span>
        </span>

      </div>


    </div>

  )

}

export default Announcement
