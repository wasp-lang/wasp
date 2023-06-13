module.exports = {
  docs: [
    {
      type: "category",
      label: "Getting started",
      collapsed: false,
      items: [
        "introduction/what-is-wasp",
        "introduction/getting-started"
      ],
    },
    {
      type: "category",
      label: "Tutorials",
      collapsed: false,
      items: [
        {
          type: "category",
          label: "Todo app",
          collapsed: true,
          items: [
            "tutorials/todo-app",
            {
              type: "category",
              label: "Basics",
              collapsed: true,
              items: [
                "tutorials/todo-app/01-creating-new-project",
                "tutorials/todo-app/02-task-entity",
                "tutorials/todo-app/03-listing-tasks",
                "tutorials/todo-app/04-creating-tasks",
                "tutorials/todo-app/05-updating-tasks",
              ],
            },
            "tutorials/todo-app/06-auth",
            "tutorials/todo-app/07-dependencies",
            "tutorials/todo-app/08-the-end",
          ],
        },
      ],
    },
    "examples",
    {
      type: "category",
      label: "Guides",
      collapsed: false,
      items: [
        "guides/auth-ui",
        {
          type: "category",
          label: "Auth Providers",
          collapsed: false,
          items: [
            "integrations/github",
            "integrations/google",
            "guides/email-auth",
          ],
        },
        "integrations/css-frameworks",
        "deploying",
        "typescript",
        "guides/testing",
        "guides/sending-emails",
        "guides/middleware-customization",
      ],
    },
    {
      type: "category",
      label: "Language",
      collapsed: false,
      items: ["language/overview", "language/syntax", "language/features"],
    },
    "cli",
    {
      type: "category",
      label: "Other",
      collapsed: true,
      items: ["contributing", "vision", "telemetry", "contact"],
    },
  ],
};
