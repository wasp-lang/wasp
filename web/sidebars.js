module.exports = {
  docs: [
    {
      type: 'category',
      label: 'Introduction',
      collapsed: false,
      items: [
        'getting-started',
        'about',
        'how-it-works',
        'pick-a-tutorial'
      ]
    },
    {
      type: 'category',
      label: 'Tutorials',
      collapsed: false,
      items: [
        {
          type: 'category',
          label: 'Todo app',
          collapsed: true,
          items: [
            'tutorials/todo-app',
            {
              type: 'category',
              label: 'Basics',
              collapsed: true,
              items: [
                'tutorials/todo-app/01-creating-new-project',
                'tutorials/todo-app/02-task-entity',
                'tutorials/todo-app/03-listing-tasks',
                'tutorials/todo-app/04-creating-tasks',
                'tutorials/todo-app/05-updating-tasks'
              ]
            },
            'tutorials/todo-app/06-auth',
            'tutorials/todo-app/07-dependencies',
            'tutorials/todo-app/08-the-end'
          ]
        },
        {
          type: 'category',
          label: 'Dev Excuses app',
          collapsed: true,
          items: [
            'tutorials/dev-excuses-app',
            'tutorials/dev-excuses-app/01-creating-the-project',
            'tutorials/dev-excuses-app/02-modifying-main-wasp-file',
            'tutorials/dev-excuses-app/03-adding-operations',
            'tutorials/dev-excuses-app/04-updating-main-page-js-file',
            'tutorials/dev-excuses-app/05-perform-migration-and-run',
          ]
        }
      ]
    },
    {
      type: 'category',
      label: 'Language',
      collapsed: false,
      items: [
        'language/overview',
        'language/syntax',
        'language/features'
      ]
    },
    'cli',
    'deploying',
    'examples',
    {
      type: 'category',
      label: 'Integrations',
      collapsed: false,
      items: [
        'integrations/google'
      ]
    },
    {
      type: 'category',
      label: 'Other',
      collapsed: false,
      items: [
        'contributing',
        'vision',
        'telemetry',
        'contact'
      ]
    }
  ]
}
