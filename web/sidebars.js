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
                'tutorials/todo-app/creating-new-project',
                'tutorials/todo-app/task-entity',
                'tutorials/todo-app/listing-tasks',
                'tutorials/todo-app/creating-tasks',
                'tutorials/todo-app/updating-tasks'
              ]
            },
            'tutorials/todo-app/auth',
            'tutorials/todo-app/dependencies',
            'tutorials/todo-app/the-end'
          ]
        },
        {
          type: 'category',
          label: 'Dev Excuses app',
          collapsed: true,
          items: [
            'tutorials/dev-excuses-app',
            'tutorials/dev-excuses-app/creating-the-project',
            'tutorials/dev-excuses-app/modifying-main-wasp-file',
            'tutorials/dev-excuses-app/adding-operations',
            'tutorials/dev-excuses-app/updating-main-page-js-file',
            'tutorials/dev-excuses-app/perform-migration-and-run',
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
