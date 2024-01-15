import React from 'react'
import classNames from 'classnames'
import { Terminal, Layers, Coffee, Code } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

import styles from '../pages/styles.module.css'

const Lang = () => (
  <>
    <span className="font-bold underline decoration-yellow-500">language</span>
  </>
)

const Benefit = ({ Icon, title, description }) => (
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
  </div>
)

const Benefits = () => {
  return (
    <SectionContainer className="space-y-16">
      <div className="grid grid-cols-12">
        <div className="col-span-12 text-center">
          <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
            Get started in minutes - zero config required.
          </h2>
          <p className="text-neutral-500">
            Wasp provides all the best practices out-of-the-box and lets you
            focus on your code.
          </p>
        </div>
      </div>

      <dl className="grid grid-cols-1 md:gap-16 lg:grid-cols-3 lg:gap-x-8 xl:gap-x-24">
        <Benefit
          Icon={Layers}
          title="Truly full-stack"
          description={`
            When we say full-stack, we really mean it. Wasp has you covered from front-end,
            back-end and database to deployment. Zero config required to get started.
          `}
        />

        <Benefit
          Icon={Coffee}
          title="The wheel can take a break"
          description={`
            No reinventing the wheel here. Write your code in React & Node.js as you are used to,
            along with your favourite NPM packages.
          `}
        />

        <Benefit
          Icon={Code}
          title="Less boilerplate"
          description={`
            The .wasp config file allows us to immensely improve developer experience.
            E.g., full-stack auth takes only 5 lines of code.
          `}
        />
      </dl>
    </SectionContainer>
  )
}

const BenefitsWithSkewedBorder = () => (
  <div className="relative">
    <div className={classNames(styles.sectionSkewedContainer)}>
      <div
        className={classNames(
          styles.sectionSkewed,
          'border-b border-t border-yellow-500/25 bg-neutral-100/50'
        )}
      ></div>
    </div>
    <div className="relative">
      <Benefits />
    </div>
  </div>
)

export default BenefitsWithSkewedBorder
