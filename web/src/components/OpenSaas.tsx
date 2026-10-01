import Link from "@docusaurus/Link";
import { ReactNode } from "react";
import { Search } from "react-feather";

import CtaLink from "./CtaLink";
import SectionContainer from "./Layouts/SectionContainer";
import SectionLabel from "./Layouts/SectionLabel";

const OpenSaas = () => (
  <SectionContainer>
    <SectionLabel
      text="starter template"
      bgColorClassName="bg-wasp-purple"
      textColorClassName="text-wasp-white"
    />

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

          <div className="flex gap-8">
            <Stat number="14k+" label="GitHub stars" />
            <Stat number="1k+" label="apps launched" />
            <Stat
              number="#2"
              label="on Product Hunt"
              to="https://www.producthunt.com/products/open-saas"
            />
          </div>

          <div className="mt-12">
            <CtaLink to="https://opensaas.sh" variant="purple">
              Explore Open SaaS <Search size={18} />
            </CtaLink>
          </div>
        </div>

        <ul>
          <Feature>
            <Strong>Authentication</Strong> (email, Google, GitHub)
          </Feature>
          <Feature>
            <Strong>Stripe</Strong> payments & subscriptions
          </Feature>
          <Feature>
            <Strong>Admin</Strong> dashboard
          </Feature>
          <Feature>
            Markdown-based <Strong>blog</Strong> (powered by Astro)
          </Feature>
          <Feature>
            <Strong>Email</Strong> sending (SendGrid, Mailgun, Resend)
          </Feature>
          <Feature>
            <Strong>File upload</Strong> (AWS S3)
          </Feature>
          <Feature>
            <Strong>SEO</Strong> optimized
          </Feature>
          <Feature>
            <Strong>Analytics</Strong> integration (GA, Plausible)
          </Feature>
          <Feature>
            <Strong>Landing page</Strong> template
          </Feature>
          <Feature>
            <Strong>OpenAI API</Strong> integration
          </Feature>
          <Feature>
            Full <Strong>test</Strong> suite
          </Feature>
        </ul>
      </div>
    </div>
  </SectionContainer>
);

const Stat = ({
  number,
  label,
  to,
}: {
  number: string;
  label: string;
  to?: string;
}) => {
  const inner = (
    <>
      <div className="font-mono text-2xl font-extrabold text-wasp-purple">
        {number}
      </div>
      <div className="mt-0.5 font-mono text-[0.65rem] uppercase tracking-wider text-wasp-g5">
        {label}
      </div>
    </>
  );
  return to ? (
    <Link to={to} className="transition duration-200 ease-out hover:opacity-70">
      {inner}
    </Link>
  ) : (
    <div>{inner}</div>
  );
};

const Feature = ({ children }: { children: ReactNode }) => (
  <li className="flex items-center gap-2.5 border-b-2 border-wasp-black/15 py-2 font-mono text-sm text-wasp-g6 last:border-b-0">
    <span aria-hidden="true" className="font-bold text-wasp-purple">
      ✓
    </span>
    <span>{children}</span>
  </li>
);

const Strong = ({ children }: { children: ReactNode }) => (
  <strong className="font-bold text-wasp-purple">{children}</strong>
);

export default OpenSaas;
