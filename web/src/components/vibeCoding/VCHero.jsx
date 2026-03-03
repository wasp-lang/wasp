import Link from "@docusaurus/Link";
import { ArrowUpRight, Terminal, ChevronDown } from "react-feather";
import CodeHighlight from "../CodeHighlight";
import SectionContainer from "../Layouts/SectionContainer";

const waspConfigSource = `// main.wasp.ts — your app's high-level blueprint
import { App } from 'wasp-config'

const app = new App('vibeSaaS', {
  title: 'My SaaS',
  auth: {
    userEntity: 'User',
    methods: { google: {}, email: {} }
  }
})

app.route('DashboardRoute', {
  path: '/dashboard', to: 'Dashboard'
})
app.page('Dashboard', {
  authRequired: true,
  component: {
    importDefault: 'Dashboard',
    from: '@src/Dashboard'
  }
})

app.query('getProjects', {
  fn: { import: 'getProjects', from: '@src/projects' },
  entities: ['Project']
})

export default app`;

const ActionButtons = () => (
  <div className="flex items-center gap-2">
    <Link to="/docs/quick-start">
      <button
        className="inline-flex items-center space-x-2 rounded border border-yellow-500 bg-yellow-500 px-3 py-2 text-sm leading-4 text-white transition duration-200 ease-out hover:border-yellow-400 hover:bg-yellow-400"
      >
        <Terminal size={16} />
        <span>Get Started</span>
      </button>
    </Link>
    <a href="#how-wasp-works">
      <button
        className="inline-flex items-center space-x-2 rounded border border-neutral-500 px-3 py-2 text-sm leading-4 text-neutral-700 transition duration-200 ease-out hover:border-neutral-400 hover:text-neutral-400"
      >
        <ChevronDown size={16} />
        <span>See How It Works</span>
      </button>
    </a>
  </div>
);

const AIToolLogos = () => (
  <div className="flex flex-col gap-4">
    <small className="text-xs text-neutral-500">Works with</small>
    <div className="flex items-center gap-6">
      <span className="text-sm text-neutral-400">Claude Code</span>
      <span className="text-sm text-neutral-400">Cursor</span>
      <span className="text-sm text-neutral-400">GitHub Copilot</span>
      <span className="text-sm text-neutral-400">Windsurf</span>
    </div>
    <span className="mt-6 flex items-center">
      <small className="text-xs text-neutral-500">Backed by</small>
      <img
        className="ml-2 w-24"
        src="img/lp/yc-logo-rounded.webp"
        alt="Y Combinator"
      />
    </span>
  </div>
);

function FileViewer({ fileName, fileExplanation, children }) {
  return (
    <div className="relative flex flex-col items-center justify-center">
      <div className="flex h-6 w-full items-center justify-between rounded-t-md bg-[#F3EDE0] px-2">
        <span className="flex items-center space-x-1 text-sm text-neutral-500">
          <span>{fileName}</span>
          <span className="text-neutral-400">· {fileExplanation}</span>
        </span>
        <div className="flex space-x-2">
          <div className="h-2 w-2 rounded-full bg-yellow-500" />
          <div className="h-2 w-2 rounded-full bg-yellow-500" />
          <div className="h-2 w-2 rounded-full bg-yellow-500" />
        </div>
      </div>
      <div className="w-full rounded-b-md text-sm shadow-2xl">{children}</div>
    </div>
  );
}

const VCHero = () => {
  return (
    <SectionContainer className="pb-5 pt-24">
      <div className="lg:grid lg:grid-cols-12 lg:gap-16">
        <div className="z-10 space-y-12 lg:col-span-6">
          <div>
            <h1 className="text-4xl font-extrabold text-neutral-700 lg:text-5xl lg:leading-tight">
              The full-stack framework where AI builds{" "}
              <span className="underline decoration-yellow-500">features</span>,
              not boilerplate.
            </h1>
            <p className="mt-4 text-xl text-neutral-500 sm:mt-5 lg:text-xl">
              Wasp gives your AI assistant a high-level blueprint of your entire
              app. Architecture decisions are already made. Auth, CRUD, and
              deployment are declared in config. Your AI just writes business
              logic.
            </p>
          </div>
          <ActionButtons />
          <AIToolLogos />
        </div>
        <div className="mt-16 flex flex-col gap-4 lg:col-span-6 lg:mt-0">
          <FileViewer
            fileName="main.wasp.ts"
            fileExplanation="TypeScript config"
          >
            <CodeHighlight language="typescript" source={waspConfigSource} />
          </FileViewer>
          <p className="text-center text-sm italic text-neutral-400">
            TypeScript config your AI (and your IDE) already understands.
            Split across multiple files as your app grows.
          </p>
        </div>
      </div>
    </SectionContainer>
  );
};

export default VCHero;
