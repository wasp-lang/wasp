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

// TODO(matija): this is duplication from HowItWorks section.
const GhIssueLink = ({ url, label }) => (
  <Link to={url}>
    <span
      className={`
        cursor-pointer rounded-full
        bg-neutral-600 px-2.5
        py-1 text-xs text-white
      `}
    >
      <div className="group inline-flex items-center gap-1">
        <span>{label}</span>
        <div className="transition-all group-hover:ml-0.5">
          <span className="text-yellow-400">
            <ArrowRight size={14} strokeWidth={2} />
          </span>
        </div>
      </div>
    </span>
  </Link>
)

const Section = ({ features }) => (
  <ul className="space-y-6">
    {features.map((f) => (
      <li className="grid grid-cols-12">
        <div className="col-span-8 col-start-3 flex items-center">
          <span>
            <span className="text-neutral-600">{f[0]}</span>
            {f[1] && (
              <>
                &nbsp;
                <GhIssueLink
                  url={'https://github.com/wasp-lang/wasp/issues/' + f[1]}
                  label={f[1]}
                />
              </>
            )}
          </span>
        </div>
      </li>
    ))}
  </ul>
)

const Roadmap = () => (
  <SectionContainer className="lg:py-18 space-y-16" id="roadmap">
    <div className="grid grid-cols-12">
      <div className="col-span-12 text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          🚧 Roadmap 🚧
        </h2>
        <p className="text-neutral-500">
          Work on Wasp never stops: get a glimpse of what is coming next!
        </p>
      </div>
    </div>

    <div className="grid grid-cols-1 md:gap-16 lg:grid-cols-2">
      <div
        className={`
            rounded-lg border border-yellow-500/25
            bg-yellow-500/5 p-5
          `}
      >
        <div className="mb-6 text-center font-bold text-neutral-700">
          Near-term improvements and features
        </div>
        <Section
          features={[
            ['Improve Wasp project structure', 734],
            ['Allow custom steps in the build pipeline', 906],
            ['Support for SSR / SSG', 911],
            ['Automatic generation of API for Operations', 863],
            ['Better Prisma support (more features, IDE)', 641],
            ['Support for backend testing', 110],
            ['Better way to define JS dependencies', 243],
          ]}
        />
      </div>

      <div
        className={`
            mt-6 rounded-lg border
            border-yellow-500/25 bg-yellow-500/20
            p-5 lg:mt-0
          `}
      >
        <div className="mb-6 text-center font-bold text-neutral-700">
          Advanced Features
        </div>
        <Section
          features={[
            ['Top-level data schema', 642],
            ['Automatic generation of CRUD UI', 489],
            ['Multiple targets (e.g. mobile)', 1088],
            ['Multiple servers, serverless'],
            ['Polyglot'],
            ['Multiple frontend libraries'],
            ['Full-stack modules'],
          ]}
        />
      </div>
    </div>
  </SectionContainer>
)

export default Roadmap
