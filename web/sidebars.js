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
      ]
    },
    'examples',
    { 
      type: 'category',
      label: 'Guides',
      collapsed: false,
      items: [
        {
          type: 'category',
          label: 'Authentication',
          collapsed: false,
          items: [
            'integrations/github',
            'integrations/google',
            'username-password'
          ]
        },
        'integrations/css-frameworks',
        'deploying',
      ],
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
    {
      type: 'category',
      label: 'Other',
      collapsed: true,
      items: [
        'contributing',
        'vision',
        'telemetry',
        'contact'
      ]
    }
  ]
}
