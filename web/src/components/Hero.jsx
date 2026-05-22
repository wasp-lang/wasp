import { useState } from "react";
import { BookOpen, Terminal } from "react-feather";
import Link from "@docusaurus/Link";

import CodeHighlight from "./CodeHighlight";
import SectionContainer from "./Layouts/SectionContainer";

const installCmd = "npm i -g @wasp.sh/wasp-cli@latest";

const InstallCommand = () => {
  const [copied, setCopied] = useState(false);

  const handleCopy = () => {
    navigator.clipboard.writeText(installCmd);
    setCopied(true);
    setTimeout(() => setCopied(false), 1500);
  };

  return (
    <button
      type="button"
      onClick={handleCopy}
      className="cursor-pointer border-0 bg-transparent p-0 text-left font-mono text-xs text-wasp-g5 transition-colors hover:text-wasp-g7"
      title="Click to copy"
      aria-label={`Copy install command: ${installCmd}`}
    >
      $ {installCmd} {copied ? "✓" : ""}
    </button>
  );
};

const ActionButtons = () => (
  <div className="flex items-center gap-2">
    <Link
      to="/docs/quick-start"
      className="inline-flex items-center space-x-2 border-2 border-wasp-black bg-wasp-yellow px-3 py-2 text-sm font-semibold leading-4 text-wasp-black transition duration-200 ease-out hover:bg-wasp-yellow-dark hover:text-wasp-black"
    >
      <Terminal size={16} />
      <span>Get Started</span>
    </Link>

    <Link
      to="/docs"
      className="inline-flex items-center space-x-2 border border-wasp-g3 px-3 py-2 text-sm leading-4 text-wasp-g6 transition duration-200 ease-out hover:border-wasp-g5 hover:text-wasp-g7"
    >
      <BookOpen size={16} />
      <span>Documentation</span>
    </Link>
  </div>
);

const codeTabs = [
  {
    name: "main.wasp",
    language: "wasp",
    source: `app todoApp {
  wasp: { version: "^0.23.0" },
  title: "ToDo App",
  auth: {
    userEntity: User,
    methods: { google: {}, gitHub: {}, email: {} },
    onAuthFailedRedirectTo: "/login"
  }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  authRequired: true,
  component: import { MainPage } from "@src/MainPage" // <-- React
}

query getTasks {
  fn: import { getTasks } from "@src/tasks", // <-- Node.js
  entities: [Task] // <-- Automatic cache invalidation.
}`,
  },
  {
    name: "schema.prisma",
    language: "prisma",
    source: `model User {
  id    Int     @id @default(autoincrement())
  email String  @unique
  tasks Task[]
}

model Task {
  id          Int     @id @default(autoincrement())
  description String
  isDone      Boolean @default(false)
  user        User    @relation(fields: [userId], references: [id])
  userId      Int
}`,
  },
  {
    name: "MainPage.tsx",
    language: "tsx",
    source: `import { getTasks, useQuery } from "wasp/client/operations"

export function MainPage() {
  const { data: tasks } = useQuery(getTasks)

  return (
    <div>
      <h1>Tasks</h1>
      {tasks?.map(task => (
        <div key={task.id}>{task.description}</div>
      ))}
    </div>
  )
}`,
  },
  {
    name: "tasks.ts",
    language: "typescript",
    source: `import { type GetTasks } from "wasp/server/operations"
import { HttpError } from "wasp/server"
import { Task } from "wasp/entities"

export const getTasks: GetTasks<void, Task[]> = async (_args, context) => {
  if (!context.user) throw new HttpError(401)

  return context.entities.Task.findMany({
    where: { user: { id: context.user.id } },
    orderBy: { id: "desc" },
  })
}`,
  },
];

function TabbedCodeViewer() {
  const [activeTab, setActiveTab] = useState(0);

  return (
    <div className="flex h-full w-full flex-col border border-wasp-g3">
      {/* Tab bar */}
      <div className="flex border-b border-wasp-g3">
        {codeTabs.map((t, i) => (
          <button
            key={t.name}
            onClick={() => setActiveTab(i)}
            className={`px-4 py-2 font-mono text-xs transition-colors ${
              i === activeTab
                ? "font-semibold text-wasp-black"
                : "text-wasp-g5 hover:text-wasp-g7"
            }`}
          >
            {i === activeTab ? (
              <>
                [<span className="bg-wasp-yellow-light">{t.name}</span>]
              </>
            ) : (
              t.name
            )}
          </button>
        ))}
      </div>
      {/* Code block — all tabs rendered, inactive ones invisible, CSS grid
          stacking ensures the container always matches the tallest tab. */}
      <div className="grid flex-1 min-w-0">
        {codeTabs.map((t, i) => (
          <div
            key={t.name}
            className={`col-start-1 row-start-1 min-w-0 text-sm ${
              i !== activeTab ? "invisible" : ""
            }`}
          >
            <CodeHighlight language={t.language} source={t.source} />
          </div>
        ))}
      </div>
    </div>
  );
}

const Hero = () => {
  return (
    <SectionContainer className="pb-5 pt-24 lg:pb-12">
      <div className="xl:grid xl:grid-cols-12 xl:gap-16">
        <div className="z-10 flex flex-col justify-between gap-12 xl:col-span-6 xl:min-w-0">
          {/* Hero title and subtitle */}
          <div>
            <div className="mb-7 inline-block border border-wasp-g3 px-3 py-1 text-[11px] uppercase tracking-[3px] text-wasp-g7">
              Full-Stack Framework for the AI Era
            </div>
            <h1
              className={`text-4xl font-extrabold uppercase leading-[1.2] tracking-[-2px] text-wasp-black lg:text-[52px]`}
            >
              Develop your full-stack app{" "}
              <span className="bg-[linear-gradient(to_top,transparent_8%,#F5C842_8%,#F5C842_48%,transparent_48%)] -mx-1 px-1 leading-[1.1]">fast</span> and{" "}
              <span className="bg-[linear-gradient(to_top,transparent_8%,#F5C842_8%,#F5C842_48%,transparent_48%)] -mx-1 px-1 leading-[1.1]">
                keep control
              </span>
              .
            </h1>

            <p className="mt-6 max-w-[460px] text-base leading-[1.75] text-wasp-g6">
              Rails-like framework for React, Node.js and Prisma. Build your app
              in a day and deploy it with a single CLI command.
            </p>
          </div>{" "}
          {/* EOF Hero title and subtitle */}
          <div>
            <ActionButtons />
            <div className="mt-4">
              <InstallCommand />
            </div>
          </div>
        </div>
        <div className="mt-16 flex w-full xl:col-span-6 xl:-ml-8 xl:mt-0 xl:min-w-0 xl:w-[calc(100%+2rem)]">
          <TabbedCodeViewer />
        </div>
      </div>

      {/* 1-min video */}
      {/*
      <div className='flex justify-center mt-20'>
        <div className='w-full lg:w-2/3 xl:w-3/5'>
          <div
            className="relative w-full rounded-md shadow-lg"
            style={{ padding: '56.25% 0 0 0' }}
          >
            <iframe
              title="Demo video showcasing Wasp"
              className="absolute h-full w-full rounded-md"
              src="https://www.youtube-nocookie.com/embed/YaaTJOhx68I?playlist=YaaTJOhx68I&autoplay=0&loop=1&controls=0&showinfo=1&modestbranding=0&rel=0&disablekb=0&mute=1"
              style={{ top: 0, left: 0 }}
              frameBorder="0"
              allow="autoplay; modestbranding; encrypted-media"
            />
          </div>
        </div>
      </div>
      */}

      {/* PH & YC badges */}
      {/*
      <div className='flex justify-center items-center space-x-4 mt-20 mb-10 md:mt-28 md:mb-0'>
        <PHBadge />
        <div
          className={`
            h-11 border border-transparent border-l-neutral-400/50
          `}
        />
        <img
          className='w-32 md:w-[180px]'
          src='img/lp/yc-logo.webp'
          alt='YC'
        />
      </div>
      */}
    </SectionContainer>
  );
};

export default Hero;
