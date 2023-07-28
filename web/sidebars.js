module.exports = {
  docs: [
    {
      type: "category",
      label: "Getting Started",
      collapsed: false,
      items: [
        "introduction/what-is-wasp",
        "introduction/getting-started",
        "introduction/editor-setup",
      ],
    },
    {
      type: "category",
      label: "Essentials",
      collapsed: false,
      items: [
        "essentials/create",
        "essentials/project-structure",
        "essentials/pages",
        "essentials/entities",
        "essentials/queries",
        "essentials/actions",
        "essentials/auth"
      ]
    },
    {
      type: "category",
      label: "Data Model",
      collapsed: false,
      items: [
        "database/entities",
        "database/operations",
        "database/crud",
        "database/backends",
      ]
    },
    {
      type: "category",
      label: "Advanced Features",
      collapsed: false,
      items: [
        "advanced/email",
        "advanced/jobs",
        "advanced/web-sockets",
        "advanced/apis",
        "advanced/middleware-config",
        "advanced/deployment"
      ]
    },
    {
      type: "category",
      label: "Authentication",
      collapsed: false,
      items: [
        "auth/overview",
        "auth/ui",
        "auth/username-and-pass",
        "auth/email",
        {
          type: "category",
          label: "Social Auth",
          collapsed: true,
          items: [
            "auth/social-auth/overview",
            "auth/social-auth/github",
            "auth/social-auth/google"
          ]
        },
      ]
    },
    {
      type: "category",
      label: "Project Setup",
      collapsed: false,
      items: [
        "project/starter-templates",
        "project/client-config",
        "project/server-config",
        "project/public-files",
        "project/dotenv",
        "project/testing",
        "project/dependencies",
        "project/css-frameworks",
      ]
    },
    {
      type: "category",
      label: "Miscellaneous",
      collapsed: false,
      items: [
        "contributing",
        "telemetry",
        "vision",
        "contact"
      ]
    }
  ],
};
