import type * as Preset from "@docusaurus/preset-classic";
import type { Config, DocusaurusConfig } from "@docusaurus/types";
import { themes } from "prism-react-renderer";
import autoImportTabs from "./src/remark/auto-import-tabs";
import autoJSCode from "./src/remark/auto-js-code";
import codeWithHole from "./src/remark/code-with-hole";
import fileExtSwitcher from "./src/remark/file-ext-switcher";
import searchAndReplace from "./src/remark/search-and-replace";

const lightCodeTheme = themes.github;

const includeCurrentVersion =
  process.env.DOCS_INCLUDE_CURRENT_VERSION === "true";
const isProduction = process.env.NODE_ENV === "production";

const config: Config = {
  title: "Wasp",
  tagline:
    "A simple language for developing full-stack web apps with less code.",
  // url, baseUrl, organizationName, projectName and trailingSlash are set according to the
  // instructions in https://docusaurus.io/docs/deployment#deploying-to-github-pages .
  url: "https://wasp.sh",
  baseUrl: "/", // Should be name of repo if hosted on Github Pages, but can be just '/' if custom domain is used.
  organizationName: "wasp-lang", // Should be GitHub org/user name if hosted on Github Pages.
  projectName: "wasp", // Should be repo name if hosted on Github Pages.
  trailingSlash: false,
  onBrokenLinks: "throw",
  onBrokenAnchors: "throw",
  onBrokenMarkdownLinks: "warn",
  favicon: "img/favicon.ico",
  themeConfig: {
    announcementBar: {
      id: "design-aithon",
      content:
        '<b>Have a Wasp app in production?</b> 🐝 <a href="https://e44cy1h4s0q.typeform.com/to/EPJCwsMi">We\'ll send you some swag! 👕</a>',
      backgroundColor: "#8b5cf6",
      textColor: "#fff",
      isCloseable: false,
    },

    imageZoom: {
      // CSS selector to apply the plugin to, defaults to '.markdown img'
      //selector: '.markdown img',
      // Optional medium-zoom options
      // see: https://www.npmjs.com/package/medium-zoom#options
      options: {
        /*
        margin: 24,
        background: '#BADA55',
        scrollOffset: 0,
        container: '#zoom-container',
        template: '#zoom-template',
        */
      },
    },
    navbar: {
      title: "Wasp (beta)",
      logo: {
        alt: "Wasp logo",
        src: "img/wasp-logo-eqpar-circle.png",
        href: "https://wasp.sh/",
        target: "_self",
      },
      items: [
        {
          type: "docsVersion",
          position: "left",
          label: "Docs",
          className: "navbar-item-docs navbar-item-outside",
        },
        {
          type: "docsVersionDropdown",
          position: "left",
          className: "navbar-item-docs-version-dropdown",
        },
        {
          to: "blog",
          label: "Blog",
          position: "left",
        },
        {
          href: "https://github.com/wasp-lang/wasp",
          className: "navbar-item-github",
          position: "right",
        },
        {
          href: "https://twitter.com/WaspLang",
          className: "navbar-item-twitter",
          position: "right",
        },
        {
          href: "https://discord.gg/rzdnErX",
          className: "navbar-item-discord",
          position: "right",
        },
      ],
    },
    prism: {
      additionalLanguages: ["shell-session", "haskell", "markdown"],
      theme: lightCodeTheme,
    },
    footer: {
      style: "dark",
      links: [
        {
          title: "Docs",
          items: [
            {
              label: "Getting started",
              to: "docs",
            },
            {
              label: "Todo app tutorial",
              to: "docs/tutorial/create",
            },
          ],
        },
        {
          title: "Community",
          items: [
            {
              label: "Discord",
              href: "https://discord.gg/rzdnErX",
            },
          ],
        },
        {
          title: "More",
          items: [
            {
              label: "GitHub",
              href: "https://github.com/wasp-lang/wasp",
            },
            {
              label: "Contact",
              to: "docs/contact",
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Wasp.`,
    },
    algolia: {
      appId: "2EJBEYUJK0",
      apiKey: "b90c50ab126398470abd0efa5d397870",
      indexName: "wasp",
      // ContextualSearch is useful when you are doing versioning,
      // it searches only in v1 docs if you are searching from v1 docs.
      // Therefore we have it enabled, since we have multiple doc versions.
      contextualSearch: true,
    },
    image: "img/wasp_twitter_cover.png",
    metadata: [{ name: "twitter:card", content: "summary_large_image" }],
  } as Preset.ThemeConfig,
  presets: [
    [
      "@docusaurus/preset-classic",
      {
        docs: {
          sidebarPath: "./sidebars.ts",
          sidebarCollapsible: true,
          editUrl: "https://github.com/wasp-lang/wasp/edit/release/web",
          remarkPlugins: [
            autoJSCode,
            autoImportTabs,
            fileExtSwitcher,
            searchAndReplace,
            codeWithHole,
          ],

          // ------ Configuration for multiple docs versions ------ //

          // "current" docs (under /docs) are in-progress docs, so we show them only in development.
          includeCurrentVersion,
          // In development, we want "current" docs to be the default docs (served at /docs),
          // to make it easier for us a bit. Otherwise, by default, the latest versioned docs
          // will be served under /docs.
          lastVersion: includeCurrentVersion ? "current" : undefined,

          // Uncomment line below to build and show only current docs, for faster build times
          // during development, if/when needed.
          // onlyIncludeVersions: includeCurrentVersion ? ["current"] : undefined,

          // "versions" option here enables us to customize each version of docs individually,
          // and there are also other options if we ever need to customize versioned docs further.
          versions: {
            // We provide config for `current` only during development because otherwise
            // we don't even build them (due to includeCurrentVersion above), so this config
            // would cause error in that case.
            ...(includeCurrentVersion
              ? {
                  current: {
                    label: "Next", // Label shown in the documentation to address this version of docs.
                    noIndex: true, // these are un-released docs, we don't want search engines indexing them.
                  },
                }
              : {}),

            // Configuration example:
            // "0.11.1": {
            //   path: "0.11.1",  // default, but can be anything.
            //   label: "0.11.1",  // default, but can be anything.
            //   banner: "unmaintained"
            //   // and more!
            // },
          },

          // ------------------------------------------------------ //
        },
        blog: {
          showReadingTime: true,
          blogSidebarCount: "ALL",
          blogSidebarTitle: "All our posts",
          postsPerPage: "ALL",
          editUrl: "https://github.com/wasp-lang/wasp/edit/release/web",
          onUntruncatedBlogPosts: "throw",
        },
        theme: {
          customCss: ["./src/css/custom.css"],
        },
      } as Preset.Options,
    ],
  ],
  scripts: getScripts(),
  plugins: [
    "plugin-image-zoom",
    async function myPlugin(context, options) {
      return {
        name: "docusaurus-tailwindcss",
        configurePostCss(postcssOptions) {
          // Appends TailwindCSS and AutoPrefixer.
          postcssOptions.plugins.push(require("tailwindcss"));
          postcssOptions.plugins.push(require("autoprefixer"));
          return postcssOptions;
        },
      };
    },
  ],
};

function getScripts() {
  const sharedScripts: DocusaurusConfig["scripts"] = [
    "/js/fix-multiple-trailing-slashes.js",
  ];

  const devOnlyScripts: DocusaurusConfig["scripts"] = [];

  const prodOnlyScripts: DocusaurusConfig["scripts"] = [
    { src: "/scripts/posthog.js", defer: true },
    // Using Cloudflare Workers to proxy the analytics script
    {
      src: "/waspara/wasp/script.js",
      defer: true,
      "data-domain": "wasp.sh",
      "data-api": "/waspara/wasp/event",
    },
  ];

  if (isProduction) {
    return [...sharedScripts, ...prodOnlyScripts];
  } else {
    return [...sharedScripts, ...devOnlyScripts];
  }
}

export default config;
