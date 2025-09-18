import Link from "@docusaurus/Link";

import SectionContainer from "./Layouts/SectionContainer";
import SubscribeForm from "./SubscribeForm";

const docs = [
  {
    text: "Getting Started",
    url: "/docs",
  },
  {
    text: "Todo app tutorial",
    url: "/docs/tutorial/create",
  },
  {
    text: "Language reference",
    url: "/docs/general/language",
  },
];

const community = [
  {
    text: "Discord",
    url: "https://discord.gg/rzdnErX",
  },
  {
    text: "X / Twitter",
    url: "https://x.com/WaspLang",
  },
  {
    text: "Bluesky",
    url: "https://bsky.app/profile/wasp.sh",
  },
  {
    text: "GitHub",
    url: "https://github.com/wasp-lang/wasp",
  },
];

const company = [
  {
    text: "Blog",
    url: "/blog",
  },
  {
    text: "Careers",
    url: "https://wasp-lang.notion.site/Wasp-Careers-59fd1682c80d446f92be5fa65cc17672",
  },
  {
    text: "Company",
    url: "https://wasp-lang.notion.site/Framework-Engineer-at-Wasp-12a18a74854c80de9481c33ebe9cccff?pvs=25#1371801161fd404a8c583cde3611238e",
  },
];

// TODO(matija): duplication, I already have Logo in Nav/index.js
const Logo = () => (
  <div className="flex flex-shrink-0 items-center">
    <Link to="/">
      <img src="img/lp/wasp-logo.webp" width={35} height={35} alt="Wasp Logo" />
    </Link>
    <span className="ml-3 text-lg font-semibold text-neutral-700">Wasp</span>
  </div>
);

const Segment = ({ title, links }) => (
  <div>
    <h6 className="text-neutral-700">{title}</h6>
    <ul className="mt-4 space-y-2">
      {links.map((l, idx) => {
        return (
          <li key={idx}>
            <a
              href={l.url}
              className={`text-sm text-neutral-500 transition-colors hover:text-neutral-400`}
            >
              {l.text}
            </a>
          </li>
        );
      })}
    </ul>
  </div>
);

const Footer = () => {
  return (
    <footer className="border-t">
      <SectionContainer>
        <div className="grid grid-cols-1 gap-8 xl:grid xl:grid-cols-3">
          {/* cols with links */}
          <div className="grid grid-cols-1 xl:col-span-2">
            <div className="grid grid-cols-2 gap-8 md:grid-cols-3">
              <Segment title="Docs" links={docs} />
              <Segment title="Community" links={community} />
              <Segment title="Company" links={company} />
            </div>
          </div>

          {/* newsletter part */}
          <div className="xl:col-span-1">
            <h3 className="text-base text-neutral-700">Stay up to date</h3>
            <p className="mt-4 text-sm text-neutral-500">
              Join our mailing list and be the first to know when we ship new
              features and updates!
            </p>

            <SubscribeForm
              className="mt-4 sm:max-w-md"
              inputBgColor="bg-transparent"
            />

            <span className="mt-6 flex items-center">
              <small className="text-xs text-neutral-500">Backed by</small>
              <img
                className="ml-2 w-24"
                src="img/lp/yc-logo-rounded.webp"
                alt="YC"
              />
            </span>
          </div>
        </div>
        <div className="mt-8 pt-8">
          <Logo />
          <div className="flex justify-between">
            <p className="mt-4 text-xs text-neutral-400">
              © Wasp, Inc. All rights reserved.
            </p>
            {/* <DarkModeToggle /> */}
          </div>
        </div>
      </SectionContainer>
    </footer>
  );
};

export default Footer;
