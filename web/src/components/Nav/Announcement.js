import React from 'react'
import classNames from 'classnames'
import { useHistory } from '@docusaurus/router'
import { ChevronRight, X } from 'react-feather'

import styles from '../../pages/styles.module.css'

const Announcement = () => {
  let history = useHistory()

  const handleLink = () => {
    //history.push('/blog/2023/04/13/db-start-and-seed')
    //history.push('/#signup')

    //window.open('https://twitter.com/MatijaSosic/status/1646532181324603395')
    window.open('https://twitter.com/WaspLang/status/1647979490180575234')
  }

  return (
    <div
      onClick={handleLink}
      className={classNames(styles.gradientBackground, `
        overflow-hidden
        cursor-pointer flex-row
        space-x-3
        text-white
      `)}
    >
      <div
        className={`
          mx-auto flex items-center justify-center divide-white p-3
          text-sm font-medium
          lg:container lg:divide-x lg:px-16 xl:px-20
        `}
      >
        <span className='item-center flex gap-2 px-3'>

          <span>Launch Week 2 is under way! 🚀</span>
        </span>

        <span className='hidden items-center space-x-2 px-3 lg:flex'>
          <span
            className={`
              bg-neutral-700 px-2.5 py-1 text-xs rounded-full cursor-pointer
              hover:bg-neutral-600
            `}
          >
            Day 5: Testing🧪 + Full-stack Type Safety🛡
          </span>
        </span>

      </div>


    </div>

  )

}

export default Announcement
