import { useState } from "react";
import CodeHighlight from "../CodeHighlight";

const tabs = [
  {
    label: "Auth Setup",
    without: {
      language: "javascript",
      source: `// auth.js — Google OAuth with Passport.js
import passport from 'passport';
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
    with: {
      language: "javascript",
      source: `app.auth({
  userEntity: User,
  methods: {
    google: {},
  },
  onAuthFailedRedirectTo: '/login',
})`,
    },
  },
  {
    label: "Type-Safe RPC",
    without: {
      language: "typescript",
      source: `// --- Server: trpc/context.ts ---
import { prisma } from '../db';
import { getSession } from 'next-auth/react';
import type { inferAsyncReturnType } from '@trpc/server';

export const createContext = async ({ req, res }) => {
  const session = await getSession({ req });
  return { prisma, user: session?.user ?? null };
};
export type Context = inferAsyncReturnType<typeof createContext>;

// --- Server: trpc/router.ts ---
import { initTRPC, TRPCError } from '@trpc/server';
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
export type AppRouter = typeof appRouter;

// --- Server: pages/api/trpc/[trpc].ts ---
import { createNextApiHandler } from '@trpc/server/adapters/next';
import { appRouter } from '../../../trpc/router';
import { createContext } from '../../../trpc/context';

export default createNextApiHandler({
  router: appRouter,
  createContext,
});

// --- Client: utils/trpc.ts ---
import { createTRPCReact } from '@trpc/react-query';
import { httpBatchLink } from '@trpc/client';
import type { AppRouter } from '../../server/trpc/router';

export const trpc = createTRPCReact<AppRouter>();

// --- Client: _app.tsx ---
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
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
}

// --- Client: TasksPage.tsx ---
import { trpc } from '../utils/trpc';

export const TasksPage = () => {
  const { data: tasks } = trpc.getTasks.useQuery();
}`,
    },
    with: {
      language: "typescript",
      source: `// --- Config: main.wasp.ts ---
app.query('getTasks', {
  fn: { import: 'getTasks', from: '@src/queries' },
  entities: ['Task']
});

// --- Server: operations.ts ---
import type { GetTasks } from 'wasp/server/operations'
import type { Task } from 'wasp/entities'

export const getTasks: GetTasks<void, Task[]> = async (args, context) => {
  return context.entities.Task.findMany({
    where: { userId: context.user.id },
    orderBy: { createdAt: 'desc' },
  })
}

// --- Client: TasksPage.tsx ---
import { useQuery, getTasks } from 'wasp/client/operations'

export const TasksPage = () => {
  const { data: tasks } = useQuery(getTasks)
  //     ^? Task[]  — auto-typed, auto-cached
}`,
    },
  },
  {
    label: "Cron Jobs",
    without: {
      language: "javascript",
      source: `// --- Server: lib/pgboss.ts ---
import PgBoss from 'pg-boss';

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
}

// --- Server: index.ts (Express boot) ---
import express from 'express';
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
start();

// --- Server: jobs/index.ts ---
import type PgBoss from 'pg-boss';
import { sendDigest } from './email';

export async function registerWorkers(boss: PgBoss) {
  await boss.schedule('email-digest', '0 7 * * *');
  await boss.work('email-digest', sendDigest);
}`,
    },
    with: {
      language: "javascript",
      source: `// --- Config ---
app.job(emailDigest, {
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
  },
];

const CodeComparison = () => {
  const [activeTab, setActiveTab] = useState(0);
  const current = tabs[activeTab];

  return (
    <div className="mx-auto max-w-6xl px-6 pb-20">
      {/* Tab bar */}
      <div className="mb-6 flex justify-center gap-2">
        {tabs.map((tab, i) => (
          <button
            key={tab.label}
            onClick={() => setActiveTab(i)}
            className={`rounded-full px-4 py-1.5 text-sm font-medium transition-colors ${
              i === activeTab
                ? "bg-yellow-500 text-white"
                : "bg-neutral-100 text-neutral-500 hover:bg-neutral-200"
            }`}
          >
            {tab.label}
          </button>
        ))}
      </div>

      {/* Panels */}
      <div className="grid grid-cols-1 gap-6 lg:grid-cols-2">
        {/* Without Wasp */}
        <div className="overflow-hidden rounded-lg border border-neutral-200 shadow-sm">
          <div className="flex items-center justify-between bg-neutral-100 px-4 py-2">
            <span className="text-sm font-medium text-neutral-500">
              Without Wasp
            </span>
            <div className="flex space-x-1.5">
              <div className="h-2.5 w-2.5 rounded-full bg-neutral-300" />
              <div className="h-2.5 w-2.5 rounded-full bg-neutral-300" />
              <div className="h-2.5 w-2.5 rounded-full bg-neutral-300" />
            </div>
          </div>
          <div className="overflow-x-auto text-sm">
            <CodeHighlight
              language={current.without.language}
              source={current.without.source}
            />
          </div>
        </div>

        {/* With Wasp */}
        <div className="overflow-hidden rounded-lg border border-yellow-300 shadow-sm">
          <div className="flex items-center justify-between bg-yellow-50 px-4 py-2">
            <span className="text-sm font-medium text-yellow-700">
              With Wasp
            </span>
            <div className="flex space-x-1.5">
              <div className="h-2.5 w-2.5 rounded-full bg-yellow-400" />
              <div className="h-2.5 w-2.5 rounded-full bg-yellow-400" />
              <div className="h-2.5 w-2.5 rounded-full bg-yellow-400" />
            </div>
          </div>
          <div className="overflow-x-auto text-sm">
            <CodeHighlight
              language={current.with.language}
              source={current.with.source}
            />
          </div>
        </div>
      </div>
    </div>
  );
};

export default CodeComparison;