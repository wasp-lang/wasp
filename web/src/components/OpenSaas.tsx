import Link from "@docusaurus/Link";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const stats = [
  { n: "14k+", l: "GitHub stars" },
  { n: "1k+", l: "apps launched" },
  {
    n: "#2",
    l: "on Product Hunt",
    to: "https://www.producthunt.com/products/open-saas",
  },
];

// Each feature is split into [before, keyword, after]; the keyword is highlighted.
const features: [string, string, string][] = [
  ["", "Authentication", " (email, Google, GitHub)"],
  ["", "Stripe", " payments & subscriptions"],
  ["", "Admin", " dashboard"],
  ["", "Blog", " with markdown"],
  ["", "Email", " sending (SendGrid, Mailgun)"],
  ["", "SEO", " optimized"],
  ["", "Analytics", " integration"],
  ["", "Landing page", " template"],
  ["", "OpenAI API", " integration"],
  ["Full ", "test", " suite"],
];

const OpenSaas = () => (
  <SectionContainer>
    <SectionLabel text="starter template" variant="purple" />

    <div className="border-2 border-wasp-purple p-6 lg:p-10">
      <div className="grid grid-cols-1 items-center gap-8 lg:grid-cols-2 lg:gap-12">
        <div>
          <h2 className="mb-3 font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-3xl">
            Start even faster with{" "}
            <span className="inline-block bg-wasp-purple px-1.5 text-wasp-white">
              Open SaaS
            </span>
          </h2>
          <p className="mb-6 max-w-prose text-pretty font-mono text-sm leading-relaxed text-wasp-g6">
            What batteries we couldn't fit into Wasp, we put into Open SaaS! The
            most popular free, open-source SaaS starter on the internet, built
            on top of Wasp. Everything you need to launch a SaaS product: wired
            up, tested, and ready to go.
          </p>

          <Link
            to="https://opensaas.sh"
            className="inline-flex items-center gap-2 border-2 border-wasp-black bg-wasp-purple px-5 py-2 font-mono text-sm font-bold text-wasp-white transition duration-200 ease-out hover:bg-wasp-purple-dark hover:text-wasp-white"
          >
            explore open saas →
          </Link>

          <div className="mt-8 flex gap-8">
            {stats.map((s) => {
              const inner = (
                <>
                  <div className="font-mono text-2xl font-extrabold text-wasp-purple">
                    {s.n}
                  </div>
                  <div className="mt-0.5 font-mono text-[0.65rem] uppercase tracking-wider text-wasp-g5">
                    {s.l}
                  </div>
                </>
              );
              return s.to ? (
                <Link
                  key={s.l}
                  to={s.to}
                  className="transition duration-200 ease-out hover:opacity-70"
                >
                  {inner}
                </Link>
              ) : (
                <div key={s.l}>{inner}</div>
              );
            })}
          </div>
        </div>

        <ul>
          {features.map(([before, keyword, after]) => (
            <Feature key={keyword} before={before} keyword={keyword} after={after} />
          ))}
        </ul>
      </div>
    </div>
  </SectionContainer>
);

const Feature = ({
  before,
  keyword,
  after,
}: {
  before: string;
  keyword: string;
  after: string;
}) => (
  <li className="flex items-center gap-2.5 border-b-2 border-wasp-black/15 py-2 font-mono text-sm text-wasp-g6 last:border-b-0">
    <span aria-hidden="true" className="font-bold text-wasp-purple">
      ✓
    </span>
    <span>
      {before}
      <span className="font-bold text-wasp-purple">{keyword}</span>
      {after}
    </span>
  </li>
);

export default OpenSaas;
