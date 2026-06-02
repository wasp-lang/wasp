import Link from "@docusaurus/Link";
import { ReactNode } from "react";

import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const stats = [
  { n: "13k+", l: "GitHub stars" },
  { n: "1k+", l: "apps launched" },
  { n: "#1", l: "on Product Hunt" },
];

const features = [
  "Authentication (email, Google, GitHub)",
  "Stripe payments & subscriptions",
  "Admin dashboard",
  "Blog with markdown",
  "Email sending (SendGrid, Mailgun)",
  "SEO optimized",
  "Analytics integration",
  "Landing page template",
  "OpenAI API integration",
  "Full test suite",
];

const OpenSaas = () => (
  <SectionContainer>
    <SectionLabel text="open saas" />

    <div className="border-2 border-wasp-black bg-wasp-yellow-light p-6 lg:p-10">
      <div className="grid grid-cols-1 items-center gap-8 lg:grid-cols-2 lg:gap-12">
        <div>
          <h2 className="mb-3 font-mono text-2xl font-extrabold tracking-tight text-wasp-black lg:text-3xl">
            Start even faster with{" "}
            <span className="inline-block bg-wasp-yellow px-1.5">
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
            className="inline-flex items-center gap-2 border-2 border-wasp-black bg-wasp-yellow px-5 py-2 font-mono text-sm font-bold text-wasp-black transition duration-200 ease-out hover:bg-wasp-yellow-dark hover:text-wasp-black"
          >
            explore open saas →
          </Link>

          <div className="mt-8 flex gap-8">
            {stats.map((s) => (
              <div key={s.l}>
                <div className="font-mono text-2xl font-extrabold text-wasp-black">
                  {s.n}
                </div>
                <div className="mt-0.5 font-mono text-[0.65rem] uppercase tracking-wider text-wasp-g5">
                  {s.l}
                </div>
              </div>
            ))}
          </div>
        </div>

        <ul className="border-t-2 border-wasp-black">
          {features.map((feature) => (
            <Feature key={feature}>{feature}</Feature>
          ))}
        </ul>
      </div>
    </div>
  </SectionContainer>
);

const Feature = ({ children }: { children: ReactNode }) => (
  <li className="flex items-center gap-2.5 border-b-2 border-wasp-black/15 py-2 font-mono text-sm text-wasp-g6">
    <span aria-hidden="true" className="font-bold text-wasp-yellow-dark">
      ✓
    </span>
    {children}
  </li>
);

export default OpenSaas;
