import React from 'react'
import Link from '@docusaurus/Link'
import classNames from 'classnames'

import SectionContainer from './Layouts/SectionContainer'

const VideoAndTutorial = () => {
  return (
    <SectionContainer id="demo">
      {/* 1-min video - a hacky way to hide controls */}
      <div className="flex justify-center md:mt-10">
        <div className="flex w-full flex-col items-center gap-6">
          <div className="w-full overflow-hidden rounded-md shadow-lg lg:w-2/3 xl:w-3/5">
            <div className="pt-25px relative -left-[50%] w-[200%] pb-[56.25%]">
              <iframe
                src="https://www.youtube-nocookie.com/embed/YaaTJOhx68I?playlist=YaaTJOhx68I&autoplay=0&loop=1&controls=0&showinfo=1&modestbranding=0&rel=0&disablekb=0&mute=1"
                title="Demo video showcasing Wasp"
                className="absolute left-0 top-0 h-full w-full rounded-md"
                frameborder="0"
                allow="accelerometer; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share; allowfullscreen;"
                allowfullscreen
              />
            </div>
          </div>

          <Link to="/docs/tutorial/create">
            <div
              className={`
                text-md text-neutral-500
                hover:text-neutral-400
              `}
            >
              <span className="underline decoration-neutral-500">
                Want to jump in and try it yourself? Take our tutorial!
              </span>
            </div>
          </Link>
        </div>
      </div>
    </SectionContainer>
  )
}

export default VideoAndTutorial
