import type * as Preset from "@docusaurus/preset-classic";
import type { Config, DocusaurusConfig } from "@docusaurus/types";
import { themes } from "prism-react-renderer";
import { SCRIPT_WITH_CONSENT_TYPE } from "./src/lib/cookie-consent";
import autoImportTabs from "./src/remark/auto-import-tabs";
import autoJSCode from "./src/remark/auto-js-code";
import codeWithHole from "./src/remark/code-with-hole";
import fileExtSwitcher from "./src/remark/file-ext-switcher";
import fixAPILinks from "./src/remark/fix-api-links";
import searchAndReplace from "./src/remark/search-and-replace";

const lightCodeTheme = {
  ...themes.github,
  plain: { ...themes.github.plain, backgroundColor: "var(--wasp-code-bg)" },
};

const darkCodeTheme = {
  ...themes.dracula,
  plain: { ...themes.dracula.plain, backgroundColor: "#1e1e1e" },
};

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
  favicon: "img/favicon.svg",
  themeConfig: {
    colorMode: {
      respectPrefersColorScheme: true,
    },

    announcementBar: {
      id: "lw12-ts-spec",
      content:
        '🦋 <b>Launch Week #12 — TS Spec</b> · Wasp goes fully TypeScript-native. <a href="/blog/2026/06/05/wasp-launch-week-12-ts-spec">Kickoff Mon, Jun 15 →</a>',
      backgroundColor: "#111",
      textColor: "#f5c842",
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
      title: "Wasp",
      logo: {
        alt: "Wasp logo",
        src: "img/wasp-logo.svg",
        href: "https://wasp.sh/",
        target: "_self",
      },
      items: [
        {
          type: "docSidebar",
          position: "left",
          sidebarId: "docs",
          label: "Docs",
          className: "navbar-item-docs navbar-item-outside",
        },
        {
          type: "docSidebar",
          position: "left",
          sidebarId: "api",
          label: "API",
        },
        {
          type: "docSidebar",
          position: "left",
          sidebarId: "guides",
          label: "Guides",
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
      additionalLanguages: [
        "shell-session",
        "haskell",
        "markdown",
        "diff",
        "bash",
      ],
      theme: lightCodeTheme,
      darkTheme: darkCodeTheme,
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
            fixAPILinks,
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
          id: "blog",
          path: "./blog",
          routeBasePath: "blog",
          blogTitle: "Blog",
          blogDescription:
            "The latest updates, tutorials, and deep-dives from the Wasp team.",
          showReadingTime: true,
          showLastUpdateTime: true,
          blogSidebarCount: "ALL",
          blogSidebarTitle: "All our posts",
          postsPerPage: "ALL",
          editUrl: "https://github.com/wasp-lang/wasp/edit/release/web",
          onUntruncatedBlogPosts: "throw",
          exclude: ["**/CLAUDE.md", "_wasp-intro.md"],
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

    [
      "@docusaurus/plugin-content-blog",
      {
        id: "resources",
        path: "./resources",
        routeBasePath: "resources",
        blogTitle: "Resources",
        blogDescription: "Guides, comparisons, and other resources.",
        authorsMapPath: "../blog/authors.yml",
        showReadingTime: true,
        showLastUpdateTime: true,
        blogSidebarCount: "ALL",
        blogSidebarTitle: "All resources",
        postsPerPage: "ALL",
        editUrl: "https://github.com/wasp-lang/wasp/edit/release/web",
        onUntruncatedBlogPosts: "throw",
      },
    ],

    async function tailwindPlugin(context, options) {
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

    [
      "docusaurus-plugin-typedoc",
      {
        // docusaurus-plugin-typedoc options
        sidebar: { typescript: true },

        // typedoc-plugin-markdown options
        readme: "none", // Otherwise it will copy the `<repo>/README.md` file to the docs, which we don't want.
        alwaysCreateEntryPointModule: true, // Otherwise it will put all of the exports of the packages into a single pool instead of per-package.

        // input packages
        entryPointStrategy: "packages",
        entryPoints: ["../waspc/data/packages/spec"],

        // If you want to set an option to a specific package, you can create a
        // `typedoc.jsonc` file in that package's folder with the desired
        // options from
        // https://typedoc.org/documents/Options.Package_Options.html.
      },
    ],
  ],
  themes: ["@docusaurus/theme-mermaid"],
  markdown: {
    mermaid: true,
    mdx1Compat: {
      admonitions: true,
      comments: true,
      headingIds: true,
    },
    hooks: {
      onBrokenMarkdownLinks: "warn",
    },
  },
  future: {
    v4: true,
  },
};

type DocusaurusScript = DocusaurusConfig["scripts"][number];
type ScriptWithConsent = Exclude<DocusaurusScript, string> & {
  requiresConsent: boolean;
};

function getScripts() {
  const sharedScripts: ScriptWithConsent[] = [
    {
      src: "/js/fix-multiple-trailing-slashes.js",
      requiresConsent: false,
    },
  ];

  const devOnlyScripts: ScriptWithConsent[] = [];

  const prodOnlyScripts: ScriptWithConsent[] = [
    {
      // We are using Cloudflare Workers to proxy the Plausible script.
      src: "/waspara/wasp/script.js",
      defer: true,
      "data-domain": "wasp.sh",
      "data-api": "/waspara/wasp/event",
      // Plausible doesn't use cookies, so it can be loaded right away.
      requiresConsent: false,
    },
    {
      src: "/scripts/posthog.js",
      defer: true,
      requiresConsent: true,
    },
    {
      src: "/scripts/reo.js",
      defer: true,
      requiresConsent: true,
    },
  ];

  const scripts = [
    ...sharedScripts,
    ...(isProduction ? prodOnlyScripts : devOnlyScripts),
  ];

  return mapToDocusaurusScripts(scripts);
}

function mapToDocusaurusScripts(
  scripts: ScriptWithConsent[],
): DocusaurusScript[] {
  return scripts.map(({ requiresConsent, ...rest }) => ({
    ...rest,
    // Scripts requiring consent shouldn't be loaded until the cookies are accepted,
    // so we use a custom type on the script _not to load it_ automatically.
    // Later, if cookies are accepted, the script will be loaded by the
    // `src/components/CookieConsentBanner.tsx` component.
    ...(requiresConsent ? { type: SCRIPT_WITH_CONSENT_TYPE } : {}),
  }));
}

export default config;
