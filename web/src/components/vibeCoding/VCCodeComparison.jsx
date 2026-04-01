import { useState } from "react";
import Link from "@docusaurus/Link";
import { ArrowRight } from "react-feather";
import CodeHighlight from "../CodeHighlight";

const tabs = [
  {
    label: "Auth Setup",
    without: {
      language: "javascript",
      files: [
        {
          name: "auth.js",
          source: `import passport from 'passport';
import { Strategy as GoogleStrategy } from 'passport-google-oauth20';
import session from 'express-session';
import RedisStore from 'connect-redis';
import csrf from 'csurf';

app.use(session({
  store: new RedisStore({ client: redisClient }),
  secret: process.env.SESSION_SECRET,
  resave: false,
  saveUninitialized: false,
  cookie: { secure: true, httpOnly: true, maxAge: 86400000 },
}));

app.use(passport.initialize());
app.use(passport.session());
app.use(csrf());

passport.serializeUser((user, done) => done(null, user.id));
passport.deserializeUser(async (id, done) => {
  const user = await prisma.user.findUnique({ where: { id } });
  done(null, user);
});

passport.use(new GoogleStrategy({
    clientID: process.env.GOOGLE_CLIENT_ID,
    clientSecret: process.env.GOOGLE_CLIENT_SECRET,
    callbackURL: '/auth/google/callback',
  },
  async (accessToken, refreshToken, profile, done) => {
    let user = await prisma.user.upsert({
      where: { email: profile.emails[0].value },
      update: { name: profile.displayName },
      create: {
        email: profile.emails[0].value,
        name: profile.displayName,
      },
    });
    return done(null, user);
  }
));

app.get('/auth/google',
  passport.authenticate('google', {
    scope: ['profile', 'email']
  })
);

app.get('/auth/google/callback',
  passport.authenticate('google', {
    failureRedirect: '/login'
  }),
  (req, res) => res.redirect('/')
);`,
        },
      ],
    },
    with: {
      language: "javascript",
      files: [
        {
          name: "main.wasp.ts",
          source: `app.auth({
  userEntity: User,
  methods: {
    google: {},
  },
  onAuthFailedRedirectTo: '/login',
})`,
        },
      ],
    },
  },
  {
    label: "Type-Safe RPC",
    without: {
      language: "typescript",
      files: [
        {
          name: "trpc/context.ts",
          source: `import { prisma } from '../db';
import { getSession } from 'next-auth/react';
import type { inferAsyncReturnType } from '@trpc/server';

export const createContext = async ({ req, res }) => {
  const session = await getSession({ req });
  return { prisma, user: session?.user ?? null };
};
export type Context = inferAsyncReturnType<typeof createContext>;`,
        },
        {
          name: "trpc/router.ts",
          source: `import { initTRPC, TRPCError } from '@trpc/server';
import { z } from 'zod';
import type { Context } from './context';

const t = initTRPC.context<Context>().create();

const isAuthed = t.middleware(({ ctx, next }) => {
  if (!ctx.user) throw new TRPCError({ code: 'UNAUTHORIZED' });
  return next({ ctx: { user: ctx.user } });
});
const protectedProcedure = t.procedure.use(isAuthed);

export const appRouter = t.router({
  getTasks: protectedProcedure.query(({ ctx }) =>
    ctx.prisma.task.findMany({
      where: { userId: ctx.user.id },
      orderBy: { createdAt: 'desc' },
    })
  ),
});
export type AppRouter = typeof appRouter;`,
        },
        {
          name: "pages/api/trpc/[trpc].ts",
          source: `import { createNextApiHandler } from '@trpc/server/adapters/next';
import { appRouter } from '../../../trpc/router';
import { createContext } from '../../../trpc/context';

export default createNextApiHandler({
  router: appRouter,
  createContext,
});`,
        },
        {
          name: "utils/trpc.ts",
          source: `import { createTRPCReact } from '@trpc/react-query';
import { httpBatchLink } from '@trpc/client';
import type { AppRouter } from '../../server/trpc/router';

export const trpc = createTRPCReact<AppRouter>();`,
        },
        {
          name: "_app.tsx",
          source: `import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { trpc } from '../utils/trpc';

const queryClient = new QueryClient();
const trpcClient = trpc.createClient({
  links: [httpBatchLink({ url: '/api/trpc' })],
});

export default function App({ Component, pageProps }) {
  return (
    <trpc.Provider client={trpcClient} queryClient={queryClient}>
      <QueryClientProvider client={queryClient}>
        <Component {...pageProps} />
      </QueryClientProvider>
    </trpc.Provider>
  );
}`,
        },
        {
          name: "TasksPage.tsx",
          source: `import { trpc } from '../utils/trpc';

export const TasksPage = () => {
  const { data: tasks } = trpc.getTasks.useQuery();
}`,
        },
      ],
    },
    with: {
      language: "typescript",
      files: [
        {
          name: "main.wasp.ts",
          source: `app.query('getTasks', {
  fn: { import: 'getTasks', from: '@src/queries' },
  entities: ['Task']
});`,
        },
        {
          name: "operations.ts",
          source: `import type { GetTasks } from 'wasp/server/operations'
import type { Task } from 'wasp/entities'

export const getTasks: GetTasks<void, Task[]> = async (args, context) => {
  return context.entities.Task.findMany({
    where: { userId: context.user.id },
    orderBy: { createdAt: 'desc' },
  })
}`,
        },
        {
          name: "TasksPage.tsx",
          source: `import { useQuery, getTasks } from 'wasp/client/operations'

export const TasksPage = () => {
  const { data: tasks } = useQuery(getTasks)
  //     ^? Task[]  — auto-typed, auto-cached
}`,
        },
      ],
    },
  },
  {
    label: "Cron Jobs",
    without: {
      language: "javascript",
      files: [
        {
          name: "lib/pgboss.ts",
          source: `import PgBoss from 'pg-boss';

let boss: PgBoss | null = null;

export async function getBoss() {
  if (!boss) {
    boss = new PgBoss({
      connectionString: process.env.DATABASE_URL,
      retryLimit: 3,
      expireInHours: 24,
      archiveCompletedAfterSeconds: 43200,
    });
    boss.on('error', console.error);
  }
  return boss;
}`,
        },
        {
          name: "index.ts",
          source: `import express from 'express';
import { getBoss } from './lib/pgboss';
import { registerWorkers } from './jobs';

const app = express();

async function start() {
  const boss = await getBoss();
  await boss.start();
  await registerWorkers(boss);

  process.on('SIGTERM', async () => {
    await boss.stop();
    process.exit(0);
  });

  app.listen(3000);
}
start();`,
        },
        {
          name: "jobs/index.ts",
          source: `import type PgBoss from 'pg-boss';
import { sendDigest } from './email';

export async function registerWorkers(boss: PgBoss) {
  await boss.schedule('email-digest', '0 7 * * *');
  await boss.work('email-digest', sendDigest);
}`,
        },
      ],
    },
    with: {
      language: "javascript",
      files: [
        {
          name: "main.wasp.ts",
          source: `app.job(emailDigest, {
  executor: PgBoss,
  perform: {
    fn: import { sendDigest } from '@src/jobs/email',
  },
  schedule: {
    cron: "0 7 * * *",
  },
  entities: [User, Task],
})`,
        },
      ],
    },
  },
];

const CodeWindow = ({ filename, label, language, source, theme }) => {
  const isYellow = theme === "yellow";
  return (
    <div className={`overflow-hidden ${isYellow ? "rounded-none border border-yellow-300" : "rounded-none border border-neutral-200"}`}>
      <div className={`relative flex items-center px-4 py-1.5 ${isYellow ? "bg-yellow-50" : "bg-neutral-100"}`}>
        <div className="flex items-center space-x-1.5">
          <div className={`h-2.5 w-2.5 rounded-full ${isYellow ? "bg-yellow-400" : "bg-neutral-300"}`} />
          <div className={`h-2.5 w-2.5 rounded-full ${isYellow ? "bg-yellow-400" : "bg-neutral-300"}`} />
          <div className={`h-2.5 w-2.5 rounded-full ${isYellow ? "bg-yellow-400" : "bg-neutral-300"}`} />
          {label && (
            <span className={`ml-2 text-xs ${isYellow ? "text-yellow-500" : "text-neutral-300"}`}>
              {label}
            </span>
          )}
        </div>
        {filename && (
          <span className={`absolute inset-0 flex items-center justify-center text-xs font-medium ${isYellow ? "text-yellow-600" : "text-neutral-400"} pointer-events-none`}>
            {filename}
          </span>
        )}
      </div>
      <div className="overflow-x-auto text-sm">
        <CodeHighlight language={language} source={source} />
      </div>
    </div>
  );
};

const CodeComparison = () => {
  const [activeTab, setActiveTab] = useState(0);
  const current = tabs[activeTab];

  return (
    <div className="mx-auto max-w-6xl px-6 py-16 sm:py-18 md:py-24 lg:py-24">
      <div className="mx-auto mb-10 max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          The Wasp{" "}
          <span className="underline decoration-yellow-500">
            Difference
          </span>
        </h2>
      </div>
      {/* Tab bar */}
      <div className="mb-6 flex justify-center gap-2">
        {tabs.map((tab, i) => (
          <button
            key={tab.label}
            onClick={() => setActiveTab(i)}
            className={`rounded-none px-4 py-1.5 text-sm font-medium transition-colors ${
              i === activeTab
                ? "bg-neutral-200 text-neutral-700"
                : "bg-neutral-100 text-neutral-500 hover:bg-yellow-500 hover:text-white"
            }`}
          >
            {tab.label}
          </button>
        ))}
        <button
          onClick={() => setActiveTab("more")}
          className={`rounded-none px-4 py-1.5 text-sm font-medium transition-colors ${
            activeTab === "more"
              ? "bg-neutral-200 text-neutral-700"
              : "bg-neutral-100 text-neutral-500 hover:bg-yellow-500 hover:text-white"
          }`}
        >
          and more...
        </button>
      </div>

      {activeTab === "more" ? (
        <div className="flex flex-col items-center justify-center py-16 text-center">
          <h3 className="mb-3 text-xl font-semibold text-neutral-700">
            Email sending, background jobs, CRUD, full-stack auth, and more
          </h3>
          <p className="mb-6 max-w-lg text-neutral-500">
            Wasp handles the full-stack plumbing so your AI writes features, not infrastructure. See everything that's built in.
          </p>
          <Link to="/docs">
            <span className="group inline-flex items-center gap-1.5 text-sm font-medium text-yellow-600 hover:text-yellow-500">
              Explore the docs
              <ArrowRight size={14} className="transition-transform group-hover:translate-x-0.5" />
            </span>
          </Link>
        </div>
      ) : (
        <div className="grid grid-cols-1 gap-6 lg:grid-cols-2">
          {/* Without Wasp */}
          <div className="flex flex-col gap-1">
            {current.without.files.map((file, idx) => (
              <CodeWindow
                key={idx}
                filename={file.name}
                label={idx === 0 ? "Without Wasp" : undefined}
                language={current.without.language}
                source={file.source}
                theme="neutral"
              />
            ))}
          </div>

          {/* With Wasp */}
          <div className="flex flex-col gap-1">
            {current.with.files.map((file, idx) => (
              <CodeWindow
                key={idx}
                filename={file.name}
                label={idx === 0 ? "With Wasp" : undefined}
                language={current.with.language}
                source={file.source}
                theme="yellow"
              />
            ))}
          </div>
        </div>
      )}
    </div>
  );
};

export default CodeComparison;
