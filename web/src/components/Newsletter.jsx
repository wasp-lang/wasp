import Link from "@docusaurus/Link";
import useBrokenLinks from "@docusaurus/useBrokenLinks";
import { ArrowUpRight, Mail } from "react-feather";
import SectionContainer from "./Layouts/SectionContainer";
import SubscribeForm from "./SubscribeForm";

const NEWSLETTER_ID = "signup";

// A peek at what actually goes out to subscribers - real, representative
// items (a feature, a post, a user story), shown like inbox subject lines.
const inboxItems = [
  {
    tag: "Shipped",
    to: "/blog/2026/08/03/wasp-resend-email-provider",
    text: "Wasp now supports Resend for sending emails",
  },
  {
    tag: "Deep dive",
    to: "/blog/2026/06/22/javascript-still-cant-ship-a-full-stack-module",
    text: "Why JavaScript still can't ship a full-stack module",
  },
  {
    tag: "Built with Wasp",
    to: "/blog/2026/07/20/made-with-wasp-searchcraft",
    text: "How Searchcraft built their SaaS on Wasp + Rust",
  },
];

const Newsletter = () => {
  useBrokenLinks().collectAnchor(NEWSLETTER_ID);

  return (
    <div className="bg-wasp-yellow">
      <SectionContainer id={NEWSLETTER_ID}>
        {/* NOTE(matija): Three flow items (intro, preview, form). On mobile they stack in
            this DOM order: intro -> preview -> form (see the feature, then sign up).

            On desktop, explicit grid placement restores the original
            layout: intro + form stacked on the left, preview on the right. */}
        <div className="grid grid-cols-1 gap-8 lg:grid-cols-[32rem_1fr] lg:grid-rows-[1fr_auto] lg:gap-x-20 lg:gap-y-7">
          <div className="lg:col-start-1 lg:row-start-1 lg:self-end">
            <h2 className="font-mono text-3xl font-extrabold tracking-tight text-wasp-black lg:text-4xl">
              Stay in the{" "}
              <span className="bg-wasp-black box-decoration-clone px-2 text-wasp-yellow">
                loop
              </span>
            </h2>
            <p className="mt-4 max-w-lg text-pretty font-mono text-sm leading-relaxed text-wasp-g7 lg:text-base">
              Get product updates, news, and builder stories. Handwritten by
              Wasp team, roughly monthly.
            </p>
          </div>

          {/* Newsletter preview: a lightly framed panel with a mail header, so
              it reads as a peek at the inbox without standing out. */}
          <div className="border border-wasp-black/25 bg-black/[0.05] lg:col-start-2 lg:row-span-2 lg:row-start-1">
            <div className="flex items-center gap-2 border-b border-wasp-black/15 px-4 py-2.5">
              <Mail size={15} className="text-wasp-black" />
              <span className="font-mono text-xs font-bold uppercase tracking-wide text-wasp-black">
                The Wasp Newsletter
              </span>
            </div>
            <ul>
              {inboxItems.map((item, i) => (
                <li key={item.to}>
                  <Link
                    to={item.to}
                    className={`group flex flex-col items-start gap-1.5 px-4 py-4 no-underline transition-colors duration-150 ease-out hover:bg-black/[0.05] sm:flex-row sm:items-center sm:gap-4 ${
                      i > 0 ? "border-t border-wasp-black/15" : ""
                    }`}
                  >
                    <span className="shrink-0 font-mono text-xs font-extrabold uppercase tracking-wide text-wasp-g7 sm:w-32">
                      {item.tag}
                    </span>
                    <span className="inline-flex items-center gap-1.5 font-mono text-sm leading-snug">
                      <strong className="font-bold text-wasp-black underline decoration-wasp-black decoration-2 underline-offset-2">
                        {item.text}
                      </strong>
                      <ArrowUpRight
                        size={15}
                        aria-hidden="true"
                        className="shrink-0 text-wasp-g6 transition-transform duration-150 ease-out group-hover:translate-x-0.5 group-hover:text-wasp-black"
                      />
                    </span>
                  </Link>
                </li>
              ))}
            </ul>
          </div>

          <div className="max-w-lg lg:col-start-1 lg:row-start-2">
            <SubscribeForm inputBgColor="bg-wasp-white" buttonVariant="black" />
          </div>
        </div>
      </SectionContainer>
    </div>
  );
};

export default Newsletter;
