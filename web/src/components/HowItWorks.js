import React from 'react'
import Link from '@docusaurus/Link'
import { ArrowRight } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

const Feature = ({ title, description, url }) => (
  <div className='col-span-12 md:col-span-6'>
    <div className='lg:mt-5'>
      <dt>
        <h4 className='mb-4'>
          <span className='text-neutral-700 bg-yellow-500/25 px-2 py-1 rounded'>
            { title }
          </span>
        </h4>
        <p className='text-neutral-600'>
          { description }
        </p>
        { url && <TextLink url={url} label='Learn more' /> }
      </dt>
    </div>
  </div>
)

const TextLink = ({ url, label }) => (
  <Link to={url}>
    <span
      className={`
        mt-3 block cursor-pointer text-sm
        text-neutral-600 hover:text-neutral-500
      `}
    >
      <div className='group flex gap-1 items-center'>
        <span>{label}</span>
        <div className='transition-all group-hover:ml-0.5'>
          <span className='text-yellow-600'>
            <ArrowRight size={14} strokeWidth={2} />
          </span>
        </div>
      </div>
    </span>
  </Link>
)

const HowItWorks = () => {
  return (
    <SectionContainer className='lg:pb-8'>
      <div className='grid grid-cols-12'>

        <div className='col-span-12 lg:col-span-4'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
             How does it work? 🧐
          </h2>
          <p className='text-neutral-700'>
            Given a simple <code>.wasp</code> configuration file that describes the high-level details of your web app, and <code>.js(x)/.css/...</code>, source files with your unique logic, Wasp compiler generates the full source of your web app in the target stack: front-end, back-end and deployment. <br/><br/>
            This unique approach is what makes Wasp "smart" and gives it its super powers!
          </p>

          {/* Features */}
          <div className='py-8'>
            <dl className='grid grid-cols-12 gap-y-4 md:gap-8'>

              <Feature
                title='Simple config language'
                url='/docs/language/overview'
                description='Declaratively describe high-level details of your app.'
              />

              <Feature
                title='Wasp CLI'
                url='/docs/cli'
                description='All the handy commands at your fingertips.'
              />

              <Feature
                title='React / Node.js / Prisma'
                description="You are still writing 90% of the code in your favorite technologies."
              />

              <Feature
                title='Arrivederci boilerplate'
                description='Write only the code that matters, let Wasp handle the rest.'
                url='https://www.youtube.com/watch?v=x5nsBbLvKnU'
              />

            </dl>

          </div> {/* EOF Features */}
        </div>

        <div className='col-span-12 lg:col-span-7 xl:col-span-7 xl:col-start-6'>
          <img
            className=''
            src='img/lp/wasp-compilation-diagram.png'
            alt='React'
          />
        </div>

      </div>
    </SectionContainer>

  )

}

export default HowItWorks
