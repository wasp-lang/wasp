import React, { useEffect, useState } from 'react'
import Link from '@docusaurus/Link'
import classNames from 'classnames'
import create from "@kodingdotninja/use-tailwind-breakpoint";

import resolveConfig from 'tailwindcss/resolveConfig'
import tailwindConfig from '../../tailwind.config.js'

import SectionContainer from './Layouts/SectionContainer'

// TODO(matija): extract this into a separate file so it can be reused. E.g. into hooks.js
const twConfig = resolveConfig(tailwindConfig)
const { useBreakpoint } = create(twConfig.theme.screens)

// TODO(matija): extract this as well - "types" and testimonials.
const TWITTER = 'twitter'
const PH = 'ph'

const testimonials = [
  {
    text: "I spent the one weekend building with Wasp and it was amazing, a real pleasure. I normally develop in Vue.js, but in a weekend I had time to learn Wasp, React and finish a full-stack app (only missing styling). This would have been impossible before.\n\nSo glad to see Wasp in Beta! 🍻",
    url: 'https://www.producthunt.com/posts/wasp-lang-beta?comment=2048094',
    name: 'Joan Reyero',
    handle: '@joanreyero',
    img: 'img/lp/tm/reyero.jpg',
    source: PH
  },
  {
    text: "@WaspLang has been in the back of my mind for months now. It left an impression, and I’m really not easy to impress. That’s gotta mean something… #programming #webdevelopment #FullStack",
    url: 'https://twitter.com/AttilaTheDev/status/1583530646047117317',
    name: 'Attila Vago',
    handle: '@AttilaTheDev',
    img: 'img/lp/tm/attila.jpg',
    source: TWITTER
  },
  {
    text: "If it weren't for Wasp, my app Amicus would probably have never been finished. I estimate it saved me 100+ hours from the start and I'm still amazed that I did all this work as a team-of-one. Being able to quickly change existing features and add the new ones is the biggest advantage of Wasp for me. Wasp is my favorite producthunt find!",
    url: 'https://www.producthunt.com/posts/wasp-lang-beta?comment=2048472',
    name: 'Erlis Kllogjri',
    handle: 'erlis_kllogjri',
    img: 'img/lp/tm/erlis.jpg',
    source: PH
  },
  {
    text: "When I first learned about Wasp on HN I was really excited about its DSL approach. It was amazing how fast I could get things running with Wasp - I had the first version within an hour! The language is also fairly simple and straightforward and plays well with React & Node.js + it removes a ton of boilerplate.\n\nI've used Wasp for a lot of personal projects and for some small utility tools at various workplaces. I'm looking forward to what Wasp will bring and mean not only for rapid prototyping but also for removing the friction of getting something going in the first instance, it'll save a lot of people a lot of time!",
    url: 'https://www.producthunt.com/posts/wasp-lang-beta?comment=2048168',
    name: 'Michael Curry',
    handle: '@michael_curry1',
    img: 'img/lp/tm/cursorial.jpg',
    source: PH
  },
  {
    text: "I used Wasp to win a hackathon this year - it was such a pleasure to use! I’ve done plenty of hackathons before where I’ve built small SaaS apps, and there’s just so much time wasted setting up common utilities - stuff like user management, databases, routing, etc. Wasp handled all that for me and let me build out our web app in record time.\n\nAlso, deploying the wasp app was incredibly easy - I didn’t have time to stand up full infrastructure in the 2 day hackathon and don’t have an infra/devops background, but I had something running on Netlify within an hour. Other projects at the hackathon struggled to do this, and putting access in the hands of the judges certainly helped get us 1st place.\n\n@matijash @martin_sosic keep up the great work.",
    url: 'https://www.producthunt.com/posts/wasp-lang-beta?comment=2048039',
    name: 'Julian LaNeve',
    handle: '@julian_laneve',
    img: 'img/lp/tm/jlaneve.jpg',
    source: PH
  },
  {
    text: "I was a bit reluctant to contribute at first due to the complexity project, but I was really encouraged when I saw the HF issue list, documentation for developers and the activity on Discord; all of the great feedback from the maintainers made my experience even better!",
    url: 'https://twitter.com/NeoLight1010/status/1595032349552566277',
    name: 'NeoLight1010',
    handle: '@NeoLight1010',
    img: 'img/lp/tm/neolight.jpg',
    source: TWITTER
  },
  {
    text: "Yes, @WaspLang is amazing and anyone who wants to bootstrap a full stack React app should try it! 😍",
    url: 'https://twitter.com/panphora/status/1547980602334294020',
    name: 'David Miranda',
    handle: '@panphora',
    img: 'img/lp/tm/panphora.jpg',
    source: TWITTER
  },
  {
    text: "Got my first @WaspLang PR merged. A big shoutout to the maintainers for leaving such helpful comments on my Haskell code!",
    url: 'https://twitter.com/infiniteverma/status/1582027815570264065',
    name: 'Anant Verma',
    handle: '@infiniteverma',
    img: 'img/lp/tm/infiniteverma.jpg',
    source: TWITTER
  },
]


const TestimonialCard = ({ url, text, name, handle, img, source }) => (
  <div className=''>
    <Link to={url}>
      <div className='bg-yellow-500/5 border border-yellow-500/25 rounded-md p-6 shadow-sm drop-shadow-sm'>
        {/* Header */}
        <div className='flex'>
          <img 
            className='rounded-full'
            src={img}
            width={45}
            height={45}
          />
          {/* Header right side */}
          <div className='pl-3 w-full flex justify-between'>
            <div className=''> {/* Name and handle */}
              <h6 className='font-semibold text-md text-neutral-700'>{name}</h6>
              <p className='text-sm text-neutral-500'>{handle}</p>
            </div>

            <div> {/* Twitter or PH icon */}
              {source === TWITTER && 
                <img
                  className='w-5 h-5'
                  src='img/lp/twitter-logo.png'
                />
              }
              {source === PH && 
                <img
                  className='w-5 h-5 rounded-full'
                  src='img/lp/ph-logo.png'
                />
              }
            </div>
          </div> {/* EOF Header right side */}
        </div>

        {/* Text */}
        <div className='mt-2 text-neutral-700 whitespace-pre-wrap'>
          {text}
        </div>
      </div>
    </Link>
  </div>
)

const testimonialLayoutConfig = {
  lg: { colsNum: 3, initialNumOfItemsToShow: 3, loadMoreStep: 3 },
  md: { colsNum: 2, initialNumOfItemsToShow: 4, loadMoreStep: 2 },
  mobile: { colsNum: 1, initialNumOfItemsToShow: 3, loadMoreStep: 1 },
}

const Testimonials = () => {

  const [loadMoreCount, setLoadMoreCount] = useState(0)

  const isSm = useBreakpoint('sm')
  const isMd = useBreakpoint('md')
  const isLg = useBreakpoint('lg')

  useEffect(() => {
    window.dispatchEvent(new Event("resize"));
  }, []);

  let layoutConfig = testimonialLayoutConfig.mobile
  if (isLg) { layoutConfig = testimonialLayoutConfig.lg }
  else if (isMd) { layoutConfig = testimonialLayoutConfig.md }

  const numOfItemsToShow = Math.min(
    layoutConfig.initialNumOfItemsToShow + loadMoreCount * layoutConfig.loadMoreStep,
    testimonials.length
  )

  // Data structure for rendering - assign each testimonial into the appropriate column.
  const cols = Array.from({length: layoutConfig.colsNum}, e => [])
  for (let itemIdx = 0; itemIdx < numOfItemsToShow; itemIdx++) {
    const targetCol = itemIdx % layoutConfig.colsNum
    cols[targetCol].push(testimonials[itemIdx])
  }

  return (
    <SectionContainer className='space-y-16' id='showcases'>
      <div className='grid grid-cols-12'>
        <div className='col-span-12 text-center'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
            You're in a good crowd
          </h2>
          <p className='text-neutral-500'>
            Here's what folks using Wasp say about it. Join&nbsp;
            <a 
              href='https://discord.gg/rzdnErX'
              className='underline decoration-2 decoration-yellow-500 font-medium'
            >
              our Discord
            </a> for more!
          </p>
        </div>
      </div>

      <div className='flex space-x-4'>
        {cols.map((col, colIdx) => (
          <div key={colIdx} className={`flex flex-col space-y-4 w-1/${layoutConfig.colsNum}`}>
            {col.map((item, itemIdx) => (
              <TestimonialCard
                key={itemIdx}
                {...item}
              />
            ))}
          </div>
        ))}
      </div>

      {numOfItemsToShow < testimonials.length &&
        <div className='flex justify-center'>
          <button
            className={`
              inline-flex items-center space-x-2
              px-3 py-2 rounded
              bg-yellow-500 text-white text-sm leading-4
              border border-yellow-500 hover:border-yellow-400
              hover:bg-yellow-400
              transition ease-out duration-200
            `}
            onClick={() => setLoadMoreCount(loadMoreCount + 1)}
          >
            🐝 Load more
          </button>
        </div>
      }

    </SectionContainer>
  )
}

export default Testimonials
