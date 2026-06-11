import CodeHighlight from "../CodeHighlight";
import { VCSection } from "./vcWrappers";

const waspCode = `import { app, job, page, query, route } from "@wasp.sh/spec"

import { DashboardPage } from "./src/DashboardPage" with { type: "ref" }
import { dailyReport } from "./src/jobs/report" with { type: "ref" }
import { getTasks } from "./src/queries" with { type: "ref" }

export default app({
  name: "mySaasApp",
  title: "My SaaS App",
  wasp: { version: "^0.24.0" },
  auth: {
    userEntity: "User",
    methods: { google: {}, email: {} }
  },
  emailSender: {
    provider: "SMTP",
    defaultFrom: { email: "hello@myapp.com" }
  },
  spec: [
    route("DashboardRoute", "/dashboard", page(DashboardPage)),
    query(getTasks, { entities: ["Task"] }),
    job(dailyReport, {
      executor: "PgBoss",
      schedule: { cron: "0 8 * * *" }
    })
  ]
})
`;

const bullets = [
  "Authentication (Google, email)",
  "Pages and routes",
  "Data operations (queries, actions)",
  "Recurring jobs",
  "Email sending",
];

const VCBlueprint = ({ variant }) => {
  return (
    <VCSection variant={variant}>
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          A central place for your{" "}
          <span className="underline decoration-yellow-500">entire app</span>
        </h2>
        <p className="text-neutral-500">
          Your main.wasp.ts is like a blueprint of your app. Features defined
          here get handled by Wasp, so you and your agent can focus on what
          makes your app unique.
        </p>
      </div>

      <div className="mx-auto mt-12 max-w-2xl overflow-hidden rounded-md border border-neutral-200">
        <div className="flex items-center gap-2 border-b border-neutral-200 bg-neutral-50 px-4 py-2">
          <span className="text-sm font-medium text-neutral-500">
            main.wasp.ts
          </span>
        </div>
        <CodeHighlight language="typescript" source={waspCode} />
      </div>

      <div className="mx-auto mt-8 max-w-2xl">
        <p className="mb-3 text-sm font-semibold text-neutral-700">
          At a glance you can see:
        </p>
        <ul className="grid grid-cols-2 gap-2 sm:grid-cols-4">
          {bullets.map((item) => (
            <li
              key={item}
              className="flex items-center gap-2 text-sm text-neutral-500"
            >
              <span className="inline-block h-1.5 w-1.5 rounded-full bg-yellow-500" />
              {item}
            </li>
          ))}
        </ul>
      </div>
    </VCSection>
  );
};

export default VCBlueprint;
