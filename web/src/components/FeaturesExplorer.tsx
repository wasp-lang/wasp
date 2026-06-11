import Link from "@docusaurus/Link";
import classNames from "classnames";
import {
  useState,
  type KeyboardEvent,
  type MouseEvent,
  type ReactNode,
} from "react";
import { BookOpen } from "react-feather";

import CodeHighlight from "./CodeHighlight";
import InlineCode from "./InlineCode";
import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const FeaturesExplorer = () => {
  const [activeId, setActiveId] = useState(features[0].id);

  const tabSharedClassName = classNames(
    "shrink-0 transition-colors",
    "border-wasp-g7 border-r last:border-r-0",
    "lg:w-auto lg:border-r-0 lg:border-b lg:last:border-b-0",
  );
  const tabInactiveBgHoverClass = "hover:bg-white/5";

  // Arrow-key navigation between tabs (APG tabs pattern).
  const onTabsKeyDown = (e: KeyboardEvent<HTMLDivElement>) => {
    if (!(e.target as HTMLElement).matches('[role="tab"]')) return;
    const count = features.length;
    const currIdx = features.findIndex((f) => f.id === activeId);
    let nextIdx;
    switch (e.key) {
      case "ArrowRight":
      case "ArrowDown":
        nextIdx = (currIdx + 1) % count;
        break;
      case "ArrowLeft":
      case "ArrowUp":
        nextIdx = (currIdx - 1 + count) % count;
        break;
      case "Home":
        nextIdx = 0;
        break;
      case "End":
        nextIdx = count - 1;
        break;
      default:
        return;
    }
    e.preventDefault();
    setActiveId(features[nextIdx].id);
    document.getElementById(`tab-${features[nextIdx].id}`)?.focus();
  };

  return (
    <SectionContainer>
      <SectionLabel text="features" />
      <p className="mb-6 max-w-prose text-base leading-relaxed text-wasp-g6">
        Instead of wiring everything yourself, Wasp gives you a cohesive set of
        full-stack features so you can focus on what makes your app unique.
      </p>

      <div className="grid grid-cols-1 overflow-hidden border-2 border-wasp-black bg-wasp-bg lg:h-[44rem] lg:grid-cols-[240px_1fr]">
        {/* Tab bar */}
        <div
          role="tablist"
          aria-label="Wasp features"
          onKeyDown={onTabsKeyDown}
          className={classNames(
            "tab-bar-fade-mobile flex overflow-x-auto border-b-2 border-wasp-black bg-wasp-black font-mono lg:flex-col lg:overflow-y-auto lg:border-b-0 lg:border-r-2",
            "wasp-scrollbar",
          )}
        >
          {features.map((feature) => (
            <Tab
              key={feature.id}
              feature={feature}
              isActive={feature.id === activeId}
              onSelect={() => setActiveId(feature.id)}
              className={classNames(
                tabSharedClassName,
                !(feature.id === activeId) && tabInactiveBgHoverClass,
              )}
            />
          ))}
          <Link
            to="/docs"
            className={classNames(
              tabSharedClassName,
              "flex items-center justify-center gap-2 px-4 py-3",
              tabInactiveBgHoverClass,
            )}
          >
            <span className="whitespace-nowrap text-xs font-bold text-wasp-yellow">
              ... see all in the docs
            </span>
          </Link>
        </div>

        {/* Feature preview */}
        <div
          className={classNames(
            "min-w-0 bg-wasp-bg lg:overflow-y-auto",
            "wasp-scrollbar",
          )}
        >
          {features.map((feature) => (
            <div
              key={feature.id}
              role="tabpanel"
              id={`panel-${feature.id}`}
              aria-labelledby={`tab-${feature.id}`}
              hidden={feature.id !== activeId}
              tabIndex={0}
              className="p-5 lg:p-6"
            >
              <p className="mb-5 max-w-prose text-sm leading-relaxed text-wasp-g6">
                {feature.intro}
              </p>
              <div className="space-y-5">
                {feature.codeBlocks.map((codeBlock) => (
                  <CodeBlock key={codeBlock.head} codeBlock={codeBlock} />
                ))}
              </div>
            </div>
          ))}
        </div>
      </div>
    </SectionContainer>
  );
};

const Tab = ({
  feature,
  isActive,
  onSelect,
  className,
}: {
  feature: Feature;
  isActive: boolean;
  onSelect: () => void;
  className?: string;
}) => (
  <div
    role="tab"
    id={`tab-${feature.id}`}
    tabIndex={isActive ? 0 : -1}
    aria-selected={isActive}
    aria-controls={`panel-${feature.id}`}
    onClick={onSelect}
    onKeyDown={(e: KeyboardEvent) => {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        onSelect();
      }
    }}
    className={classNames(
      className,
      "relative w-52 cursor-pointer select-none py-2 pl-4 pr-8",
      isActive && "z-10 bg-wasp-yellow",
    )}
  >
    <h4
      className={classNames(
        "truncate leading-snug",
        "text-sm font-bold",
        isActive ? "text-wasp-black" : "text-wasp-white",
      )}
    >
      {feature.title}
    </h4>
    <p
      className={classNames(
        "truncate leading-snug",
        "text-xs",
        isActive ? "text-wasp-g7" : "text-wasp-g4",
      )}
    >
      {feature.sub}
    </p>
    <Link
      to={feature.docUrl}
      onClick={(e: MouseEvent) => e.stopPropagation()}
      aria-label={`${feature.title} docs`}
      tabIndex={isActive ? 0 : -1}
      className={classNames(
        "absolute right-2.5 top-1/2 -translate-y-1/2 transition hover:scale-125",
        isActive
          ? "text-wasp-black hover:text-wasp-g7"
          : "text-wasp-yellow hover:text-wasp-yellow-dark",
      )}
    >
      <BookOpen size={14} strokeWidth={1.75} aria-hidden="true" />
    </Link>
  </div>
);

const CodeBlock = ({ codeBlock }: { codeBlock: CodeBlockInfo }) => (
  <div className="border border-wasp-black bg-wasp-bg text-xs sm:text-sm">
    <div className="border-b border-wasp-black bg-wasp-yellow-light px-4 py-2 font-mono text-xs font-semibold text-wasp-black">
      {codeBlock.head}
    </div>
    <CodeHighlight language={codeBlock.language} source={codeBlock.source} />
  </div>
);

interface CodeBlockInfo {
  head: string;
  language: string;
  source: string;
}

interface Feature {
  id: string;
  title: string;
  sub: string;
  docUrl: string;
  intro: ReactNode;
  codeBlocks: CodeBlockInfo[];
}

const features: Feature[] = [
  {
    id: "spec",
    title: "High-Level Spec",
    sub: "app, auth, route, ...",
    docUrl: "/docs/general/spec",
    intro: (
      <>
        Define your app via a specialized full-stack aware logic layer, using
        high-level terms like <InlineCode>app</InlineCode>,{" "}
        <InlineCode>route</InlineCode>, <InlineCode>page</InlineCode>, ... .
        Think framework config, but taken to a whole new level.
      </>
    ),
    codeBlocks: [
      {
        head: "main.wasp.ts",
        language: "typescript",
        source: `import { app, job, page, query, route } from "@wasp.sh/spec";

import { AdminDashboard } from "./src/admin/Dashboard" with { type: "ref" };
import { dailyDigest, getMetrics } from "./src/admin/ops" with { type: "ref" };
import { HomePage } from "./src/pages/Home" with { type: "ref" };

export default app({
  name: "TodoApp",
  title: "TodoApp",
  wasp: { version: "^0.24.0" },
  auth: {
    userEntity: "User",
    methods: { email: {}, google: {} },
    onAuthFailedRedirectTo: "/login"
  },
  spec: [
    route("HomeRoute", "/", page(HomePage)),
    route(
      "AdminRoute",
      "/admin",
      page(AdminDashboard, { authRequired: true })
    ),
    query(getMetrics, { entities: ["Task", "User"] }),
    job(dailyDigest, {
      executor: "PgBoss",
      schedule: { cron: "0 7 * * *" }
    })
  ]
});`,
      },
    ],
  },
  {
    id: "auth",
    title: "Full-Stack Auth",
    sub: "username · email · social",
    docUrl: "/docs/auth/overview",
    intro:
      "Declare the auth methods you want, Wasp does the rest. Use the high-level API of premade but customizable Login/Signup/... forms, or drop lower and implement your own.",
    codeBlocks: [
      {
        head: "main.wasp.ts",
        language: "typescript",
        source: `// ...
auth: {
  userEntity: "User",
  methods: {
    email: { fromField: { email: "hi@todo.dev" } },
    google: {}, gitHub: {}
  },
  onAuthFailedRedirectTo: "/login"
},
spec: [
  route("LoginRoute", "/login", page(LoginPage)),
  route(
    "ProfileRoute",
    "/profile",
    page(ProfilePage, { authRequired: true })
  )
]`,
      },
      {
        head: "src/auth/LoginPage.tsx",
        language: "tsx",
        source: `import { LoginForm } from "wasp/client/auth";

export default function LoginPage() {
  return <LoginForm />; // ready-made, themable, drop-in
}`,
      },
      {
        head: "src/user/ProfilePage.tsx",
        language: "tsx",
        source: `import { useAuth } from "wasp/client/auth";

export default function ProfilePage() {
  const { data: user } = useAuth();
  return <p>Hi, {user?.identities.email?.id}</p>;
}`,
      },
    ],
  },
  {
    id: "data",
    title: "Data Models",
    sub: "typed · integrated",
    docUrl: "/docs/data-model/entities",
    intro:
      "Declare your data models in Prisma, have them integrated and fully-typed through the whole stack.",
    codeBlocks: [
      {
        head: "schema.prisma",
        language: "prisma",
        source: `model Task {
  id      Int     @id @default(autoincrement())
  title   String
  done    Boolean @default(false)
  user    User    @relation(fields: [userId], references: [id])
  userId  Int
}`,
      },
      {
        head: "main.wasp.ts",
        language: "typescript",
        source: `// ...
spec: [
  query(countTasks, { entities: ["Task"] }),
  action(createTask, { entities: ["Task", "User"] }),
  job(dailyDigest, { executor: "PgBoss", entities: ["Task", "User"] })
]`,
      },
      {
        head: "src/task/operations.ts",
        language: "typescript",
        source: `// ...
export const countTasks: CountTasks<void, number> =
  async (_args, ctx) => ctx.entities.Task.count();`,
      },
      {
        head: "shell",
        language: "bash",
        source: `$ wasp db start db
$ wasp db migrate-dev | wasp db studio | wasp db seed`,
      },
    ],
  },
  {
    id: "rpc",
    title: "Type-Safe RPC",
    sub: "typed client ↔ server",
    docUrl: "/docs/data-model/operations/overview",
    intro:
      "Define functions on the server, call them from the client. No need for REST / GQL / ... . Fully typed and reactive. Powered by TanStack Query.",
    codeBlocks: [
      {
        head: "main.wasp.ts",
        language: "typescript",
        source: `// ...
spec: [
  query(countTasks, { entities: ["Task"] })
]`,
      },
      {
        head: "src/task/operations.ts",
        language: "typescript",
        source: `import type { CountTasks } from "wasp/server/operations";

export const countTasks: CountTasks<void, number> = async (_args, ctx) => {
  return ctx.entities.Task.count();
};`,
      },
      {
        head: "src/task/TaskCounter.tsx",
        language: "tsx",
        source: `import { useQuery, countTasks } from "wasp/client/operations";

export default function TaskCounter() {
  const { data: count, isLoading, error } = useQuery(countTasks);
  if (isLoading) return <p>Loading...</p>;
  if (error) return <p>Error: {error.message}</p>;
  return <p>{count} tasks in total.</p>;
}`,
      },
    ],
  },
  {
    id: "jobs",
    title: "Background Jobs",
    sub: "cron · one-off · retry",
    docUrl: "/docs/advanced/jobs",
    intro:
      "Cron jobs, one-offs, retries, all working out of the box. No external service needed.",
    codeBlocks: [
      {
        head: "main.wasp.ts",
        language: "typescript",
        source: `// ...
spec: [
  job(sendReminders, {
    executor: "PgBoss",
    schedule: { cron: "0 9 * * *" },   // every day at 9am
    entities: ["Task", "User"]
  })
]`,
      },
      {
        head: "src/reminders.ts",
        language: "typescript",
        source: `import type { SendReminders } from "wasp/server/jobs";

export const sendReminders: SendReminders<{ userId?: number }, void> =
  async ({ userId }, ctx) => {
    const tasks = await ctx.entities.Task.findMany({
      where: { done: false, ...(userId && { userId }) }
    });
    // ...send a reminder email for each unfinished task
  };`,
      },
      {
        head: "src/task/operations.ts",
        language: "typescript",
        source: `import type { CreateTask } from "wasp/server/operations";
import { sendReminders } from "wasp/server/jobs";

export const createTask: CreateTask<{ title: string }, Task> =
  async ({ title }, ctx) => {
    const task = await ctx.entities.Task.create({ data: { title, userId: ctx.user.id } });
    await sendReminders.submit({ userId: ctx.user.id }).delay(3600); // one-off, 1h later
    return task;
  };`,
      },
    ],
  },
  {
    id: "email",
    title: "Email Sending",
    sub: "provider-agnostic",
    docUrl: "/docs/advanced/email",
    intro:
      "Provider-agnostic, integrated API for easy sending of emails from your web app.",
    codeBlocks: [
      {
        head: "main.wasp.ts",
        language: "typescript",
        source: `// ...
emailSender: {
  provider: "SendGrid", // or "Mailgun" | "SMTP" | "Dummy"
  defaultFrom: { email: "hi@todo.dev", name: "TodoApp" }
},
auth: {
  // ...
  methods: {
    email: { /* ... */ }  // will automatically use the email sender
  }
}`,
      },
      {
        head: "src/user/operations.ts",
        language: "typescript",
        source: `import type { SendGreeting } from "wasp/server/operations";
import { emailSender } from "wasp/server/email";

export const sendGreeting: SendGreeting<{ userId: number }, void> =
  async ({ userId }, ctx) => {
    const user = await ctx.entities.User.findUniqueOrThrow({ where: { id: userId } });
    await emailSender.send({
      to: user.email,
      subject: "Welcome to TodoApp 🎉",
      html: "<h1>Glad you are here.</h1>"
    });
  };`,
      },
    ],
  },
  {
    id: "ws",
    title: "WebSockets",
    sub: "typed real-time",
    docUrl: "/docs/advanced/web-sockets",
    intro:
      "Make your web app alive with a fully integrated WebSocket experience (using Socket.IO).",
    codeBlocks: [
      {
        head: "main.wasp.ts",
        language: "typescript",
        source: `// ...
webSocket: { fn: webSocketFn }`,
      },
      {
        head: "src/webSocket.ts",
        language: "typescript",
        source: `import type { WebSocketDefinition } from "wasp/server/webSocket";

type WebSocketFn = WebSocketDefinition<
  { chatMessage: (msg: string) => void },                       // client → server
  { chatMessage: (m: { user: string, text: string }) => void }  // server → client
>;

export const webSocketFn: WebSocketFn = (io, context) =>
  io.on("connection", (socket) => {
    const user = socket.data.user?.identities.username?.id ?? "Anon";
    socket.on("chatMessage", (text) => io.emit("chatMessage", { user, text }));
  });`,
      },
      {
        head: "src/pages/ChatPage.tsx",
        language: "tsx",
        source: `import { useState } from "react";
import { useSocket, useSocketListener } from "wasp/client/webSocket";

export default function ChatPage() {
  const [messages, setMessages] = useState<{ user: string, text: string }[]>([]);
  const { socket } = useSocket();
  useSocketListener("chatMessage", (m) => setMessages((prev) => [...prev, m]));
  return <button onClick={() => socket.emit("chatMessage", "hi!")}>Send</button>;
}`,
      },
    ],
  },
  {
    id: "static",
    title: "Static Rendering",
    sub: "SSG for SEO & LLMs",
    docUrl: "/docs/advanced/prerendering",
    intro: (
      <>
        Set <InlineCode>prerender: true</InlineCode> to get the best speed / SEO
        / LLM experience for your static content.
      </>
    ),
    codeBlocks: [
      {
        head: "main.wasp.ts",
        language: "typescript",
        source: `// ...
spec: [
  route("HomeRoute", "/", page(HomePage), { prerender: true }),
  route("PricingRoute", "/pricing", page(PricingPage), { prerender: true }),
  route("DashboardRoute", "/dashboard", page(DashboardPage))
]`,
      },
      {
        head: "shell",
        language: "bash",
        source: `$ wasp build
  → prerendering 2 routes
  ✔ /
  ✔ /pricing`,
      },
    ],
  },
  {
    id: "deploy",
    title: "Simple Deployment",
    sub: "one command, any platform",
    docUrl: "/docs/deployment/deployment-methods/overview",
    intro:
      "No magic — you are deploying just a React / Node.js / Prisma app. Deploy with a single command to supported providers (Fly.io and Railway for now), or deploy yourself wherever you want. Comes with a generated Dockerfile to make it easier.",
    codeBlocks: [
      {
        head: "shell · Fly.io",
        language: "bash",
        source: `$ wasp deploy fly launch my-todo-app
  → setup · create-db · deploy
  → registering apps · provisioning Postgres · setting env
  ✔ live at https://my-todo-app-client.fly.dev  (2m 14s)

$ wasp deploy fly deploy
  ✔ redeployed  (38s)`,
      },
      {
        head: "shell · Railway",
        language: "bash",
        source: `$ wasp deploy railway launch my-todo-app`,
      },
      {
        head: "shell · diy",
        language: "bash",
        source: `$ wasp build  # generates deployable code in .wasp/out/
$ ls .wasp/out
Dockerfile  server/  web-app/  db/  # ship them anywhere`,
      },
    ],
  },
  {
    id: "links",
    title: "Type-Safe Links",
    sub: "typed routes & params",
    docUrl: "/docs/advanced/links",
    intro: (
      <>
        Wasp turns your route declarations into a typed{" "}
        <InlineCode>Link</InlineCode> component and{" "}
        <InlineCode>routes</InlineCode> object, so a wrong path or a missing
        param is a compile-time error. Works with route params, search query,
        hash, and optional static segments.
      </>
    ),
    codeBlocks: [
      {
        head: "main.wasp.ts",
        language: "typescript",
        source: `// ...
spec: [
  route("TaskRoute", "/task/:id", page(TaskPage))
]`,
      },
      {
        head: "src/task/TaskList.tsx",
        language: "tsx",
        source: `import { Link } from "wasp/client/router";

// ...
  <Link to="/task/:id" params={{ id: task.id }}>...</Link>   // ✓ ok
  <Link to="/tsak/:id" params={{ id: task.id }}>...</Link>   // ✗ no such route
  <Link to="/task/:id" params={{}}>...</Link>                // ✗ missing param "id"
// ...`,
      },
      {
        head: "src/task/utils.ts",
        language: "typescript",
        source: `import { routes } from "wasp/client/router";

// ...
const taskUrl = routes.TaskRoute.build({ params: { id: someTaskId } }); // e.g. "/task/1"
// ...`,
      },
    ],
  },
  {
    id: "api",
    title: "Custom HTTP API",
    sub: "REST · webhooks",
    docUrl: "/docs/advanced/apis",
    intro:
      "Define custom HTTP APIs while still being integrated with the rest of your web app (entities, auth, ...). Powered by ExpressJS.",
    codeBlocks: [
      {
        head: "main.wasp.ts",
        language: "typescript",
        source: `// ...
spec: [
  api("GET", "/api/me/stats", getUserStats, {
    entities: ["Task"],
    auth: true
  })
]`,
      },
      {
        head: "src/user/stats.ts",
        language: "typescript",
        source: `import type { GetUserStats } from "wasp/server/api";

export const getUserStats: GetUserStats = async (req, res, ctx) => {
  const numTasks = await ctx.entities.Task.count({
    where: { userId: ctx.user.id }
  });
  res.json({ numTasks });
};`,
      },
      {
        head: "src/user/Stats.tsx",
        language: "tsx",
        source: `import { api } from "wasp/client/api";

export default function Stats() {
  const load = async () => {
    const res = await api.get("/api/me/stats");
    console.log(res.data); // { numTasks: 7 }
  };
  return <button onClick={load}>Load stats</button>;
}`,
      },
    ],
  },
  {
    id: "cli",
    title: "Wasp CLI",
    sub: "create · build · deploy",
    docUrl: "/docs/general/cli",
    intro: "A single CLI to manage your whole app lifecycle.",
    codeBlocks: [
      {
        head: "shell",
        language: "bash",
        source: `$ wasp new my-app      # scaffold a new full-stack app
$ wasp start           # run client + server in dev
$ wasp start db        # spin up a local dev database
$ wasp db migrate-dev  # run migrations, and more under \`wasp db ...\`
$ wasp studio          # visualize your app's structure
$ wasp test client     # run your tests
$ wasp build           # generate production-ready code
$ wasp deploy          # ship it to your provider`,
      },
    ],
  },
];

export default FeaturesExplorer;
