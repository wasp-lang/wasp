import React from 'react'
import Link from '@docusaurus/Link'
import classNames from 'classnames'
import {
  Terminal,
  Layers,
  Coffee,
  Code,
  Unlock,
  Repeat,
  Send,
  Link2,
  Grid,
  ArrowRight,
  Globe,
  Settings,
  Mail,
  Type,
  Star,
} from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

import styles from '../pages/styles.module.css'

const Lang = () => (
  <>
    <span className="font-bold underline decoration-yellow-500">language</span>
  </>
)

const Feature = ({ Icon, title, description, url }) => (
  <div className="mb-10 space-y-4 md:mb-0">
    <div className="flex items-center">
      <div
        className={`
          inline-flex h-8 w-8 items-center
          justify-center rounded-md
          bg-neutral-700 text-yellow-500
          
        `}
      >
        <Icon size={20} />
      </div>
      <dt className="ml-4 text-neutral-700">{title}</dt>
    </div>
    <p className="text-neutral-700">{description}</p>
    <TextLink url={url} label="Learn more" />
  </div>
)

// TODO(matija): this is duplication from HowItWorks section.
const TextLink = ({ url, label }) => (
  <Link to={url}>
    <span
      className={`
        mt-3 block cursor-pointer text-sm
        text-neutral-500 hover:text-neutral-400
      `}
    >
      <div className="group flex items-center gap-1">
        <span>{label}</span>
        <div className="transition-all group-hover:ml-0.5">
          <span className="text-yellow-600">
            <ArrowRight size={14} strokeWidth={2} />
          </span>
        </div>
      </div>
    </span>
  </Link>
)

const Features = () => {
  return (
    <SectionContainer className="lg:py-18 space-y-16">
      <dl className="grid grid-cols-1 md:gap-16 lg:grid-cols-4 lg:gap-x-8 xl:gap-x-16">
        <Feature
          Icon={Star}
          title="Open Source"
          url="https://github.com/wasp-lang/wasp"
          description={`
            This is the way. Wasp is fully open-source and you're welcome to contribute!
          `}
        />

        <Feature
          Icon={Unlock}
          title="Full-stack Auth"
          url="/blog/2023/04/12/auth-ui"
          description={`
            Add login with social providers or email in a few lines of code with powerful UI helpers. No third party vendor lock-in.
          `}
        />

        <Feature
          Icon={Link2}
          title="RPC (Client <-> Server)"
          url="/docs/data-model/operations/overview"
          description={`
            Wasp provides a typesafe RPC layer that instantly brings your data models and server logic to the client.
          `}
        />

        <Feature
          Icon={Send}
          title="Simple Deployment"
          url="/docs/advanced/deployment/overview"
          description={`
            Deploy your app to any platform. Wasp offers CLI helpers for the most popular options.
          `}
        />

        <Feature
          Icon={Settings}
          title="Jobs"
          url="/docs/advanced/jobs"
          description={`
            Easily define, schedule and run specialized server tasks.
            Persistent, retryable, delayable.
          `}
        />

        <Feature
          Icon={Mail}
          title="Email Sending"
          url="/docs/advanced/email"
          description={`
            All you need to do is connect an email provider and you can send emails!
          `}
        />

        <Feature
          Icon={Type}
          title="Full-stack Type Safety"
          url="/docs/tutorial/queries#implementing-a-query"
          description={`
            Full support for TypeScript with auto-generated types that span the whole stack.
          `}
        />

        <Feature
          Icon={Grid}
          title="And More!"
          url="/docs"
          description={`
            Custom API routes, database seeding, optimistic updates, automatic cache invalidation on the client, ...
          `}
        />
      </dl>
    </SectionContainer>
  )
}

const FeaturesWithSkewedBorder = () => (
  <div className="relative">
    <div className={classNames(styles.sectionSkewedContainer)}>
      <div
        className={classNames(
          styles.sectionSkewed,
          'border-b border-yellow-500/25 bg-neutral-100/50'
        )}
      ></div>
    </div>
    <div className="relative">
      <Features />
    </div>
  </div>
)

export default FeaturesWithSkewedBorder
