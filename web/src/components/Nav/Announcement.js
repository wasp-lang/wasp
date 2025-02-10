import React from 'react'
import classNames from 'classnames'
import { useHistory } from '@docusaurus/router'
import { ChevronRight, X } from 'react-feather'

import styles from '../../pages/styles.module.css'

const Announcement = () => {
  let history = useHistory()

  const handleLink = () => {
    history.push('/blog/2025/01/09/wasp-launch-week-8')

    // window.open('https://magic-app-generator.wasp-lang.dev/')
    //window.open('https://www.producthunt.com/posts/open-saas')
    //history.push('/blog/2023/06/30/tutorial-jam')
    //history.push('/#signup')

    //window.open('https://twitter.com/MatijaSosic/status/1646532181324603395')
    //window.open('https://twitter.com/WaspLang/status/1647979490180575234')
    //window.open('https://www.producthunt.com/posts/free-saas-template-gpt-stripe-auth')
    // window.open("https://hackathon.wasp-lang.dev");
  }

  return (
    <div
      onClick={handleLink}
      className={classNames(
        styles.gradientBackground,
        `
        cursor-pointer
        flex-row space-x-3
        overflow-hidden
        text-white
      `
      )}
    >
      <div
        className={`
          mx-auto flex items-center justify-center divide-white p-3
          text-sm font-medium
          lg:container lg:divide-x lg:px-16 xl:px-20
        `}
      >
        <span className="item-center flex gap-2 px-3">
          <span>
            <b>🐝 Wasp v0.16 is here! 🐝</b>
          </span>
        </span>

        <span className="hidden items-center space-x-2 px-3 lg:flex">
          <span
            className={`
              cursor-pointer rounded-full bg-neutral-700 px-2.5 py-1 text-xs
              hover:bg-neutral-600
            `}
          >
            {/* Generate your app 🤖 → */}
            See what's new ⚙️ →
          </span>
        </span>
      </div>
    </div>
  )
}

export default Announcement
