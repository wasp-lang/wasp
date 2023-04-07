import React from 'react'
import Link from '@docusaurus/Link'
import classNames from 'classnames'
import { Terminal, Layers, Coffee, Code, Unlock, Repeat, Send, Link2, Grid, ArrowRight } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

import styles from '../pages/styles.module.css'

const Lang = () => (
  <>
    <span className='underline decoration-yellow-500 font-bold'>
      language
    </span>
  </>
)

const Feature = ({ Icon, title, description, url }) => (
  <div className='mb-10 md:mb-0 space-y-4'>
    <div className='flex items-center'>
      <div
        className={`
          inline-flex h-8 w-8 rounded-md
          items-center justify-center
          text-yellow-500 bg-neutral-700
        `}
      >
        <Icon size={20} />
      </div>
      <dt className='ml-4 text-neutral-700'>
        { title }
      </dt>
    </div>
    <p className='text-neutral-700'>
      { description }
    </p>
    <TextLink url={url} label='Learn more' />
  </div>
)

// TODO(matija): this is duplication from HowItWorks section.
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

const Features = () => {
  return (
    <SectionContainer className='space-y-16 lg:py-18'>
      <dl className='grid grid-cols-1 lg:grid-cols-4 md:gap-16 lg:gap-x-8 xl:gap-x-16'>
        <Feature
          Icon={Unlock}
          title='Full-stack Authentication'
          url='/docs/language/features#authentication--authorization'
          description={`
            Add login with social providers or email in a few lines of code with powerful UI helpers. No third party vendor lock-in.
          `}
        />

        <Feature
          Icon={Link2}
          title='No API Required'
          url='/docs/language/features#queries-and-actions-aka-operations'
          description={`
            Wasp provides a typesafe RPC layer that instantly brings your data models and server logic to the client.
          `}
        />

        <Feature
          Icon={Send}
          title='Deployment'
          url='/docs/deploying'
          description={`
            Deploy your app to any platform. Wasp offers CLI helpers for the most popular options.
          `}
        />

        <Feature
          Icon={Grid}
          title='And More'
          url='/docs/language/features'
          description={`
            Typescript support, email sending, cron jobs and custom API routes are supported out of the box.
          `}
        />
      </dl>
    </SectionContainer>
  )
}

const FeaturesWithSkewedBorder = () => (
  <div className='relative'>
    <div className={classNames(styles.sectionSkewedContainer)}>
      <div
        className={classNames(
          styles.sectionSkewed,
          'border-b border-yellow-500/25 bg-neutral-100/50'
        )}
      >
      </div>
    </div>
    <div className='relative'>
      <Features />
    </div>
  </div>
)

export default FeaturesWithSkewedBorder
