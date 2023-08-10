module.exports = {
  docs: [
    {
      type: 'category',
      label: 'Getting Started',
      collapsed: false,
      collapsible: false,
      items: [
        'introduction/what-is-wasp',
        'introduction/getting-started',
        'introduction/editor-setup',
      ],
    },
    {
      type: 'category',
      label: 'Essentials',
      collapsed: false,
      collapsible: false,
      items: [
        'essentials/create',
        'essentials/project-structure',
        'essentials/pages',
        'essentials/entities',
        'essentials/queries',
        'essentials/actions',
        'essentials/auth',
      ],
    },
    {
      type: 'category',
      label: 'Data Model',
      collapsed: false,
      collapsible: false,
      items: [
        'database/entities',
        {
          type: 'category',
          label: 'Operations',
          collapsed: true,
          items: [
            'database/operations/overview',
            'database/operations/queries',
            'database/operations/actions',
          ],
        },
        'database/crud',
        'database/backends',
      ],
    },
    {
      type: 'category',
      label: 'Advanced Features',
      collapsed: false,
      collapsible: false,
      items: [
        'advanced/email',
        'advanced/jobs',
        'advanced/web-sockets',
        'advanced/apis',
        'advanced/middleware-config',
        {
          type: 'category',
          label: 'Deployment',
          collapsed: true,
          items: [
            'advanced/deployment/overview',
            'advanced/deployment/cli',
            'advanced/deployment/manually',
          ],
        },
      ],
    },
    {
      type: 'category',
      label: 'Authentication',
      collapsed: false,
      collapsible: false,
      items: [
        'auth/overview',
        'auth/ui',
        'auth/username-and-pass',
        'auth/email',
        {
          type: 'category',
          label: 'Social Auth',
          collapsed: true,
          items: [
            'auth/social-auth/overview',
            'auth/social-auth/github',
            'auth/social-auth/google',
          ],
        },
      ],
    },
    {
      type: 'category',
      label: 'Project Setup',
      collapsed: false,
      collapsible: false,
      items: [
        'project/starter-templates',
        'project/client-config',
        'project/server-config',
        'project/public-files',
        'project/env-vars',
        'project/testing',
        'project/dependencies',
        'project/css-frameworks',
      ],
    },
    {
      type: "category",
      label: "General",
      collapsed: false,
      collapsible: false,
      items: [
        "general/language",
      ]
    },
    {
      type: "category",
      label: "Miscellaneous",
      collapsed: false,
      collapsible: false,
      items: ['contributing', 'telemetry', 'vision', 'contact'],
    },
  ],
}
