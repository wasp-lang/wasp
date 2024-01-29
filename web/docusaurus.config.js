const lightCodeTheme = require('prism-react-renderer/themes/github')

const autoImportTabs = require('./src/remark/auto-import-tabs')
const fileExtSwitcher = require('./src/remark/file-ext-switcher')

/** @type {import('@docusaurus/types').DocusaurusConfig} */
module.exports = {
  title: 'Wasp',
  tagline:
    'A simple language for developing full-stack web apps with less code.',
  // url, baseUrl, organizationName, projectName and trailingSlash are set according to the
  // instructions in https://docusaurus.io/docs/deployment#deploying-to-github-pages .
  url: 'https://wasp-lang.dev',
  baseUrl: '/', // Should be name of repo if hosted on Github Pages, but can be just '/' if custom domain is used.
  organizationName: 'wasp-lang', // Should be GitHub org/user name if hosted on Github Pages.
  projectName: 'wasp', // Should be repo name if hosted on Github Pages.
  trailingSlash: false,
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  stylesheets: [
    'https://fonts.googleapis.com/css2?family=Inter:wght@100;200;300;400;500;600;700;800;900&display=swap',
  ],
  themeConfig: {
    /*
    announcementBar: {
      id: 'Beta_is_here',
      content: 'Wasp Hackathon #1 is underway! 🚀 <a href="https://betathon.wasp-lang.dev/">Join now</a>',
      backgroundColor: '#eab307',
      textColor: '#fff',
      isCloseable: false,
    },
    */
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
      title: '.wasp (beta)',
      logo: {
        alt: 'Wasp logo',
        src: 'img/wasp-logo-eqpar-circle.png',
        href: 'https://wasp-lang.dev/',
        target: '_self',
      },
      items: [
        {
          type: 'docsVersion',
          position: 'left',
          label: 'Docs',
          className: 'navbar-item-docs navbar-item-outside',
        },
        {
          type: 'docsVersionDropdown',
          position: 'left',
          className: 'navbar-item-docs-version-dropdown',
        },
        {
          to: 'blog',
          label: 'Blog',
          position: 'left',
        },
        {
          href: 'https://github.com/wasp-lang/wasp',
          className: 'navbar-item-github',
          position: 'right',
        },
        {
          href: 'https://twitter.com/WaspLang',
          className: 'navbar-item-twitter',
          position: 'right',
        },
        {
          href: 'https://discord.gg/rzdnErX',
          className: 'navbar-item-discord',
          position: 'right',
        },
      ],
    },
    prism: {
      additionalLanguages: ['shell-session', 'haskell'],
      theme: lightCodeTheme,
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Getting started',
              to: 'docs',
            },
            {
              label: 'Todo app tutorial',
              to: 'docs/tutorial/create',
            },
          ],
        },
        {
          title: 'Community',
          items: [
            {
              label: 'Discord',
              href: 'https://discord.gg/rzdnErX',
            },
          ],
        },
        {
          title: 'More',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/wasp-lang/wasp',
            },
            {
              label: 'Contact',
              to: 'docs/contact',
            },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Wasp.`,
    },
    algolia: {
      appId: 'RG0JSZOWH4',
      apiKey: 'feaa2a25dc596d40418c82cd040e2cbe',
      indexName: 'wasp-lang',
      // ContextualSearch is useful when you are doing versioning,
      // it searches only in v1 docs if you are searching from v1 docs.
      // Therefore we have it enabled, since we have multiple doc versions.
      contextualSearch: true,
    },
    image: 'img/wasp_twitter_cover.png',
    metadata: [{ name: 'twitter:card', content: 'summary_large_image' }],
  },
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        gtag: {
          trackingID: 'GTM-PQ4JFCK',
          anonymizeIP: true,
        },
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          sidebarCollapsible: true,
          editUrl: 'https://github.com/wasp-lang/wasp/edit/release/web',
          remarkPlugins: [autoImportTabs, fileExtSwitcher],

          // ------ Configuration for multiple docs versions ------ //

          // "current" docs (under /docs) are in-progress docs, so we show them only in development.
          includeCurrentVersion: process.env.NODE_ENV === 'development',
          // In development, we want "current" docs to be the default docs (served at /docs),
          // to make it easier for us a bit. Otherwise, by default, the latest versioned docs
          // will be served under /docs.
          lastVersion:
            process.env.NODE_ENV === 'development' ? 'current' : undefined,

          // Uncomment line below to build and show only current docs, for faster build times
          // during development, if/when needed.
          // onlyIncludeVersions: process.env.NODE_ENV === 'development' ? ["current"] : undefined,

          // "versions" option here enables us to customize each version of docs individually,
          // and there are also other options if we ever need to customize versioned docs further.
          versions: {
            // We provide config for `current` only during development because otherwise
            // we don't even build them (due to includeCurrentVersion above), so this config
            // would cause error in that case.
            ...(process.env.NODE_ENV === 'development'
              ? {
                  current: {
                    label: 'Next', // Label shown in the documentation to address this version of docs.
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
          blogSidebarCount: 'ALL',
          blogSidebarTitle: 'All our posts',
          postsPerPage: 'ALL',
          editUrl: 'https://github.com/wasp-lang/wasp/edit/release/web',
        },
        theme: {
          customCss: [require.resolve('./src/css/custom.css')],
        },
      },
    ],
  ],
  scripts: ['/scripts/posthog.js', '/js/fix-multiple-trailing-slashes.js'],
  plugins: [
    'plugin-image-zoom',
    async function myPlugin(context, options) {
      return {
        name: 'docusaurus-tailwindcss',
        configurePostCss(postcssOptions) {
          // Appends TailwindCSS and AutoPrefixer.
          postcssOptions.plugins.push(require('tailwindcss'))
          postcssOptions.plugins.push(require('autoprefixer'))
          return postcssOptions
        },
      }
    },
    [
      '@docusaurus/plugin-client-redirects',
      {
        redirects: [
          {
            from: '/docs/guides/auth-ui',
            to: '/docs/auth/ui',
          },
          {
            from: '/docs/deploying',
            to: '/docs/advanced/deployment/overview',
          },
          {
            from: '/docs/guides/username-password',
            to: '/docs/auth/username-and-pass',
          },
          {
            from: '/docs/guides/websockets',
            to: '/docs/advanced/web-sockets',
          },
          {
            from: '/docs/guides/testing',
            to: '/docs/project/testing',
          },
          {
            from: '/docs/guides/middleware-customization',
            to: '/docs/advanced/middleware-config',
          },
          {
            from: '/docs/guides/email-auth',
            to: '/docs/auth/email',
          },
          {
            from: '/docs/guides/crud',
            to: '/docs/data-model/crud',
          },
          {
            from: '/docs/integrations/google',
            to: '/docs/auth/social-auth/google',
          },
          {
            from: '/docs/integrations/github',
            to: '/docs/auth/social-auth/github',
          },
          {
            from: '/docs/integrations/css-frameworks',
            to: '/docs/project/css-frameworks',
          },
          {
            from: '/docs/tutorials/todo-app',
            to: '/docs/tutorial/create',
          },
        ],
      },
    ],
  ],
}
