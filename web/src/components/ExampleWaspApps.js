import React from 'react'
import Link from '@docusaurus/Link'
import SectionContainer from './Layouts/SectionContainer'
import { GitHub, ArrowUpRight, Monitor } from 'react-feather'

const examples = [
  {
    title: 'Todo App (TypeScript) ✅',
    description: 'A famous To-Do list app, implemented in TypeScript.',
    authorName: 'wasp',
    authorImg: 'https://avatars.githubusercontent.com/u/55102317',
    repoName: 'todo-app-ts',
    repoUrl: 'https://github.com/wasp-lang/wasp/tree/main/examples/todo-app-in-typescript',
    //demoUrl: 'https://waspello-demo.netlify.app/',
    // todo: try in GitPod/Replit url
  },
  {
    title: 'CoverLetterGPT 🤖',
    description: 'Generate cover letters based on your CV and job description. Powered by ChatGPT.',
    authorName: 'vincanger',
    authorImg: 'https://avatars.githubusercontent.com/u/70215737',
    repoName: 'cover-letter-gpt',
    repoUrl: 'https://github.com/vincanger/coverlettergpt',
    demoUrl: 'https://coverlettergpt.xyz/',
  },
  {
    title: 'Realtime voting via WebSockets 🔌',
    description:
      'A realtime, websockets powered voting app built with Wasp in TypeScript.',
    authorName: 'wasp',
    authorImg: 'https://avatars.githubusercontent.com/u/55102317',
    repoName: 'waspleau-jobs-example',
    repoUrl: 'https://github.com/wasp-lang/wasp/tree/main/examples/websockets-realtime-voting',
    demoUrl: 'https://websockets-client-production.up.railway.app/login',
  },
]

const SeeTheCodeButton = ({ repoUrl }) => (
  <Link to={repoUrl}>
    <button
      className={`
        flex items-center
        text-xs
        px-2.5 py-1 rounded
        bg-transparent border border-yellow-500 text-neutral-500
        hover:text-neutral-400
        transition ease-out duration-200
      `}
    >
      <span>See the code</span>
      <ArrowUpRight className="ml-2" size={14} />
    </button>
  </Link>
)

const DemoButton = ({ demoUrl }) => (
  <Link to={demoUrl}>
    <button
      className={`
        flex items-center
        text-xs
        px-2.5 py-1 rounded
        bg-yellow-500 text-white
        hover:bg-yellow-400
        transition ease-out duration-200
      `}
    >
      <span>Demo</span>
      <Monitor className="ml-2" size={14} />
    </button>
  </Link>
)

const ExampleCard = (props) => (
  <>
    {/* Top half */}
    <div
      className={`
        bg-yellow-500/5
        border-t border-l border-r border-yellow-500/25
        rounded rounded-b-none
        flex flex-col
        h-40
        p-5
      `}
    >
      <div className="mb-4">
        <h4 className="mb-4 text-neutral-700">{props.title}</h4>
        <p className="text-sm mb-4 text-neutral-500">{props.description}</p>
        <div>
          <img
            className="inline w-6 rounded-full"
            src={props.authorImg}
            alt={props.authorName + ' GitHub profile picture'}
          />
          <span className="ml-2 text-sm text-neutral-700">
            {props.authorName}
          </span>
        </div>
      </div>
    </div>

    {/* Bottom half */}
    <div
      className={`
        bg-yellow-500/20
        border-b border-l border-r border-yellow-500/25
        rounded rounded-t-none
        flex flex-col
        p-5
      `}
    >
      <Link to={props.repoUrl}>
        <span className="flex items-center text-sm text-neutral-500 hover:text-neutral-400">
          <span className="">{props.repoName}</span>
          <span className="ml-1 inline-block">
            <GitHub className="" size={14} />
          </span>
        </span>
      </Link>

      {/* Action buttons */}
      <div className="mt-3 flex items-center gap-2">
        <SeeTheCodeButton repoUrl={props.repoUrl} />
        {/* Demo apps are not mobile-friendly yet so hiding them on mobile for now. */}
        <span className="hidden md:block">
          {props.demoUrl && <DemoButton demoUrl={props.demoUrl} />}
        </span>
      </div>
    </div>
  </>
)

const ExampleWaspApps = () => {
  return (
    <SectionContainer className="space-y-16" id="examples">
      <div className="grid grid-cols-12">
        <div className="col-span-12 text-center">
          <h2 className="text-xl lg:text-2xl text-neutral-700 mb-4">
            Show, don't tell.
          </h2>
          <p className="text-neutral-500">
            Take a look at examples - see how things work and get inspired for
            your next project.
          </p>
        </div>
      </div>

      <div className="mt-16 grid grid-cols-12 gap-5">
        {examples.slice(0, 6).map((e, idx) => (
          <div className="col-span-12 lg:col-span-6 xl:col-span-4">
            <ExampleCard {...e} />
          </div>
        ))}
      </div>

      <div className="flex justify-center">
        <Link to="https://github.com/wasp-lang/wasp/tree/main/examples">
          <span
            className={`
              text-neutral-500
              underline decoration-2 decoration-yellow-500 font-medium
              hover:text-neutral-400
              transition ease-out duration-200
              flex items-center
            `}
          >
            <span>See all examples</span>
            <ArrowUpRight className="ml-1" size={14} />
          </span>
        </Link>
      </div>
    </SectionContainer>
  )
}

export default ExampleWaspApps
