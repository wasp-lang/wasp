import React from 'react'
import Link from '@docusaurus/Link'
import classNames from 'classnames'
import { Terminal, Layers, Coffee, Code, Unlock, Repeat, Send, Link2, Grid, ArrowRight, Globe, Settings, Mail, Type, Star } from 'react-feather'

import SectionContainer from './Layouts/SectionContainer'

import styles from '../pages/styles.module.css'

// TODO(matija): this is duplication from HowItWorks section.
const GhIssueLink = ({ url, label }) => (
  <Link to={url}>
    <span className={`
        cursor-pointer text-xs
        bg-neutral-600 text-white
        px-2.5 py-1 rounded-full
      `}
    >
      <div className='group inline-flex gap-1 items-center'>
        <span>{label}</span>
        <div className='transition-all group-hover:ml-0.5'>
          <span className='text-yellow-400'>
            <ArrowRight size={14} strokeWidth={2} />
          </span>
        </div>
      </div>

    </span>
  </Link>
)

const Section = ({ features }) => (
  <ul className='space-y-6'>
    {features.map(f => (
      <li className='grid grid-cols-12'>
        <div className='flex items-center col-start-3 col-span-8'>
          <span>
            <span className='text-neutral-600'>{f[0]}</span>
            {f[1] && <>&nbsp;<GhIssueLink url={'https://github.com/wasp-lang/wasp/issues/' + f[1]} label={f[1]} /></>}
          </span>
        </div>
      </li>
    ))}
  </ul>
)

const Roadmap = () => (
    <SectionContainer className='space-y-16 lg:py-18' id='roadmap'>
      <div className='grid grid-cols-12'>
        <div className='col-span-12 text-center'>
          <h2 className='text-xl lg:text-2xl text-neutral-700 mb-4'>
            🚧 Roadmap 🚧
          </h2>
          <p className='text-neutral-500'>
            Work on Wasp never stops: get a glimpse of what is coming next!
          </p>
        </div>
      </div>

      <div className='grid grid-cols-1 lg:grid-cols-2 md:gap-16'>

        <div
          className={`
            bg-yellow-500/5 border border-yellow-500/25
            p-5 rounded-lg
          `}
        >
          <div className='font-bold text-center mb-6'>Near-term improvements and features</div>
          <Section features={[
            ['Improve Wasp project structure', 734],
            ['Allow custom steps in the build pipeline', 906],
            ['Support for SSR / SSG', 911],
            ['Automatic generation of API for Operations', 863],
            ['Better Prisma support (more features, IDE)', 641],
            ['Support for backend testing', 110],
            ['Better way to define JS dependencies', 243]
          ]} />
        </div>

        <div
          className={`
            bg-yellow-500/20 border border-yellow-500/25
            p-5 rounded-lg
            mt-6 lg:mt-0
          `}
        >
          <div className='font-bold text-center mb-6'>Advanced Features</div>
          <Section features={[
            ['Top-level data schema', 642],
            ['Automatic generation of CRUD UI', 489],
            ['Multiple targets (e.g. mobile)', 1088],
            ['Multiple servers, serverless'],
            ['Polyglot'],
            ['Multiple frontend libraries'],
            ['Full-stack modules']
          ]} />
        </div>

      </div>

    </SectionContainer>
)

export default Roadmap
