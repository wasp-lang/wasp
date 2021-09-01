const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').DocusaurusConfig} */
module.exports = {
  title: 'Wasp',
  tagline: 'A simple language for developing full-stack web apps with less code.',
  // NOTE: Although Docusaurus says that 'url' should not contain anything after the hostname,
  //   and instead that part should go into 'baseUrl', that doesn't work for us, assets are not loaded
  //   successfully. Instead, we add '/wasp/' to the end of the 'url' and we set 'baseUrl' to '/'
  //   and that works.
  url: 'https://wasp-lang.github.io/wasp/',
  baseUrl: '/', // Should be name of repo if hosted on Github Pages.
  organizationName: 'wasp-lang', // Should be GitHub org/user name if hosted on Github Pages.
  projectName: 'wasp', // Should be repo name if hosted on Github Pages.
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  trailingSlash: false,
  themeConfig: {
    sidebarCollapsible: true,
    navbar: {
      title: '.wasp (alpha)',
      logo: {
        alt: 'Wasp logo',
        src: 'img/wasp-logo-eqpar-circle.png',
      },
      items: [
        {
          to: 'docs/',
          activeBasePath: 'docs',
          label: 'Docs',
          position: 'left',
          className: 'navbar-item-docs navbar-item-outside'
        },
        {
          to: 'blog',
          label: 'Blog',
          position: 'left'
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
      additionalLanguages: ['shell-session', 'haskell']
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Docs',
          items: [
            {
              label: 'Getting started',
              to: 'docs'
            },
            {
              label: 'Todo app tutorial',
              to: 'docs/tutorials/todo-app'
            },
            {
              label: 'Reference',
              to: 'docs/language/basic-elements'
            }
          ]
        },
        {
          title: 'Community',
          items: [
            {
              label: 'Discord',
              href: 'https://discord.gg/rzdnErX'
            }
          ]
        },
        {
          title: 'More',
          items: [
            {
              label: 'GitHub',
              href: 'https://github.com/wasp-lang/wasp'
            },
            {
              label: 'Contact',
              href: 'docs/contact'
            }
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Wasp.`,
    },
    algolia: {
      apiKey: '51575685bf62ecdfd2e3a1974de921e6',
      indexName: 'wasp-lang',
      // TODO: contextualSearch is useful when you are doing versioning,
      //   it searches only in v1 docs if you are searching from v1 docs.
      //   We should enable it if we start doing versioning.
      // contextualSearch: true
    }
  },
  presets: [
    [
      '@docusaurus/preset-classic',
      {
        docs: {
          sidebarPath: require.resolve('./sidebars.js'),
          // Please change this to your repo.
          editUrl:
            'https://github.com/wasp-lang/wasp/edit/master/web',
        },
        blog: {
          showReadingTime: true,
          // Please change this to your repo.
          editUrl:
            'https://github.com/wasp-lang/wasp/edit/master/web/blog',
        },
        theme: {
          customCss: [
            require.resolve('./src/css/custom.css'),
          ]
        },
      },
    ],
  ],
  scripts: [
    '/scripts/posthog.js'
  ]
};
