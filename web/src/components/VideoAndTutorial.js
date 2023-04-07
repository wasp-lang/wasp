import React from 'react'
import Link from '@docusaurus/Link'
import classNames from 'classnames'

import SectionContainer from './Layouts/SectionContainer'

const VideoAndTutorial = () => {
  return (
    <SectionContainer id='demo'>

      {/* 1-min video - a hacky way to hide controls */}
      <div className='flex justify-center md:mt-10'>
        <div className='flex flex-col items-center w-full gap-6'>

          <div className='w-full lg:w-2/3 xl:w-3/5 overflow-hidden rounded-md shadow-lg'>
            <div className='relative w-[200%] -left-[50%] pb-[56.25%] pt-25px'>
              <iframe
                src='https://www.youtube-nocookie.com/embed/YaaTJOhx68I?playlist=YaaTJOhx68I&autoplay=0&loop=1&controls=0&showinfo=1&modestbranding=0&rel=0&disablekb=0&mute=1'
                title='Demo video showcasing Wasp'
                className='absolute h-full w-full top-0 left-0 rounded-md'
                frameborder='0'
                allow='accelerometer; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share; allowfullscreen;'
                allowfullscreen
              />
            </div>
          </div>

          <Link to='/docs/tutorials/todo-app'>
            <div
              className={`
                text-neutral-500 text-md
                hover:text-neutral-400
              `}
            >
              <span className='underline decoration-neutral-500'>Want to jump in and try it yourself? Take our tutorial!</span>
            </div>
          </Link>


        </div>
      </div>


    </SectionContainer>
  )
}

export default VideoAndTutorial
