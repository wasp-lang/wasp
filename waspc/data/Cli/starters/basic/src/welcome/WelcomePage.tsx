// A getting-started page for your new app. See "Removing the welcome page" in
// `README.md` for how to remove it once you no longer need it.

import { useAuth } from "wasp/client/auth";
import { ButtonLink } from "../shared/components/Button";
import { CodeWindow } from "./components/CodeWindow";
import { InlineCode } from "./components/InlineCode";
import { Walkthrough, type WalkthroughStep } from "./components/Walkthrough";
import { LogInStep } from "./LogInStep";

export function WelcomePage() {
  const { data, isLoading } = useAuth();
  const user = data ?? null;

  const steps: WalkthroughStep[] = [
    {
      title: "Create a user and log in",
      isDone: !!user,
      content: <LogInStep user={user} />,
      media: (
        <CodeWindow
          title="terminal"
          code={`$ wasp start
...
╔═══════════════════════╗
║ Dummy email sender    ║
╚═══════════════════════╝
From:    Basic App <hello@example.com>
To:      you@example.com
Subject: Verify your email
═════════ Text ═════════
Click the link below to verify your email: http://localhost:3000/email-verification?token=...`}
        />
      ),
    },
    {
      title: "Manage your tasks",
      content: (
        <>
          <p>
            The tasks page is the logged-in part of this app. Create some tasks
            and tags, then open <InlineCode>src/tasks/</InlineCode> to see the
            page, queries, and actions behind it.
          </p>
          <div>
            <ButtonLink to="/tasks" size="sm">
              Go to your tasks
            </ButtonLink>
          </div>
        </>
      ),
      media: (
        <CodeWindow
          title="src/tasks/task.wasp.ts"
          code={`export const tasksSpec: Spec = [
  // Only logged-in users can see this page.
  route("TasksRoute", "/tasks", page(TasksPage, { authRequired: true })),
  query(getTasks, { entities: ["Task", "Tag"] }),
  action(createTask, { entities: ["Task"] }),
  // ...
];`}
        />
      ),
    },
    {
      title: "Inspect your app and data",
      content: (
        <>
          <p>
            <InlineCode>wasp studio</InlineCode> shows a map of your app: its
            pages, routes, operations, and entities.
          </p>
          <p>
            <InlineCode>wasp db studio</InlineCode> opens Prisma Studio, where
            you can browse and edit your database, like the users you just
            created. Stop <InlineCode>wasp start</InlineCode> before running it,
            since only one Wasp command can work on a project at a time.
          </p>
        </>
      ),
      media: (
        <CodeWindow
          title="terminal"
          code={`# See how your app fits together
$ wasp studio

# Browse and edit your data
$ wasp db studio`}
        />
      ),
    },
    {
      title: "Learn the building blocks",
      content: (
        <>
          <p>
            You declare your app&apos;s features in{" "}
            <InlineCode>main.wasp.ts</InlineCode> and write the code behind them
            in <InlineCode>src/</InlineCode>. As your app grows, you can split
            the declarations into <InlineCode>*.wasp.ts</InlineCode> files next
            to their code, like <InlineCode>src/tasks/task.wasp.ts</InlineCode>.
            Pick a topic to learn more:
          </p>
          <ul className="grid grid-cols-2 gap-2">
            {docsLinks.map((link) => (
              <li key={link.href}>
                <a
                  href={link.href}
                  target="_blank"
                  rel="noreferrer"
                  className="hover:bg-primary-100 flex justify-between border border-neutral-900 px-3 py-2 font-mono text-xs font-semibold"
                >
                  {link.label} <span aria-hidden>→</span>
                </a>
              </li>
            ))}
          </ul>
        </>
      ),
      media: (
        <CodeWindow
          title="main.wasp.ts"
          code={`import { tasksSpec } from "./src/tasks/task.wasp";
// ...

export default app({
  // ...
  spec: [
    welcomeSpec,
    tasksSpec,
    tagsSpec,
    route("LoginRoute", "/login", page(LoginPage)),
    // ...
  ],
});`}
        />
      ),
    },
    {
      title: "Deploy your app",
      content: (
        <>
          <p>
            When you&apos;re ready to share your app,{" "}
            <InlineCode>wasp deploy</InlineCode> puts it online with a single
            command. Before that, switch two development defaults to their
            production versions:{" "}
            <a
              href="https://wasp.sh/docs/features/data/databases#migrating-from-sqlite-to-postgresql"
              target="_blank"
              rel="noreferrer"
              className="font-semibold underline"
            >
              move from SQLite to PostgreSQL
            </a>{" "}
            and{" "}
            <a
              href="https://wasp.sh/docs/features/email#providers"
              target="_blank"
              rel="noreferrer"
              className="font-semibold underline"
            >
              pick a real email provider
            </a>{" "}
            instead of the Dummy one.
          </p>
          <div>
            <a
              href="https://wasp.sh/docs/deployment/methods/wasp-deploy/overview"
              target="_blank"
              rel="noreferrer"
              className="bg-primary-500 hover:bg-primary-400 inline-block border-2 border-neutral-900 px-4 py-2 font-mono text-sm font-bold text-neutral-900"
            >
              Deploy your app <span aria-hidden>→</span>
            </a>
          </div>
        </>
      ),
      media: (
        <CodeWindow
          title="terminal"
          code={`# Deploy to Railway
$ wasp deploy railway launch my-wasp-app

# Or to Fly.io
$ wasp deploy fly launch my-wasp-app dfw`}
        />
      ),
    },
  ];

  return (
    <div className="flex justify-center px-6 py-12 lg:px-12">
      <div className="flex w-full max-w-(--breakpoint-lg) min-w-0 flex-col gap-10">
        <div className="flex flex-col items-start gap-4">
          <span className="border border-neutral-400 px-3 py-1 font-mono text-[11px] tracking-[3px] text-neutral-600 uppercase">
            Welcome to Wasp
          </span>
          <h1 className="text-4xl leading-tight font-extrabold tracking-tight uppercase lg:text-5xl">
            Your app is{" "}
            <span className="bg-primary-500 px-2">up and running</span>
          </h1>
          <p className="max-w-2xl text-neutral-600">
            Follow these steps to get to know your new app. This page lives in{" "}
            <InlineCode>src/welcome/</InlineCode>, so feel free to change it, or
            delete it once you&apos;re done (the{" "}
            <InlineCode>README.md</InlineCode> explains how).
          </p>
        </div>
        {/* Wait until we know whether you're logged in, so the open step
            doesn't jump. */}
        {!isLoading && <Walkthrough steps={steps} />}
      </div>
    </div>
  );
}

const docsLinks = [
  {
    label: "Pages",
    href: "https://wasp.sh/docs/features/pages-and-routes",
  },
  {
    label: "Data model",
    href: "https://wasp.sh/docs/features/data/entities",
  },
  { label: "Auth", href: "https://wasp.sh/docs/features/auth/overview" },
  { label: "Email", href: "https://wasp.sh/docs/features/email" },
  { label: "Jobs", href: "https://wasp.sh/docs/features/jobs" },
  { label: "WebSockets", href: "https://wasp.sh/docs/features/websockets" },
  { label: "API routes", href: "https://wasp.sh/docs/features/apis" },
];
