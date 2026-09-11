import type { SidebarsConfig } from "@docusaurus/plugin-content-docs";
import type {
  SidebarItemConfig,
  SidebarItemLink,
} from "@docusaurus/plugin-content-docs/src/sidebars/types.js";
import typedocSidebar from "./docs/api/typedoc-sidebar";

const sidebars: SidebarsConfig = {
  docs: [
    {
      type: "category",
      label: "Getting Started",
      collapsed: false,
      collapsible: true,
      items: [
        "introduction/introduction",
        "introduction/quick-start",
        "introduction/editor-setup",
      ],
    },
    {
      type: "category",
      label: "Tutorial",
      collapsed: false,
      collapsible: true,
      items: [
        "tutorial/create",
        "tutorial/project-structure",
        "tutorial/pages",
        "tutorial/entities",
        "tutorial/queries",
        "tutorial/actions",
        "tutorial/auth",
      ],
    },
    {
      type: "link",
      label: "Examples",
      href: "https://github.com/wasp-lang/wasp/tree/release/examples",
    },
    {
      type: "category",
      label: "Data Model",
      collapsed: false,
      collapsible: true,
      items: [
        "data-model/entities",
        {
          type: "category",
          label: "Operations",
          collapsed: true,
          items: [
            "data-model/operations/overview",
            "data-model/operations/queries",
            "data-model/operations/actions",
          ],
        },
        "data-model/crud",
        "data-model/databases",
        "data-model/prisma-file",
      ],
    },
    {
      type: "category",
      label: "Authentication",
      collapsed: false,
      collapsible: true,
      items: [
        "auth/overview",
        "auth/ui",
        {
          type: "category",
          label: "Username & Password",
          collapsed: true,
          items: [
            "auth/username-and-pass",
            "auth/username-and-pass/create-your-own-ui",
          ],
        },
        {
          type: "category",
          label: "Email",
          collapsed: true,
          items: ["auth/email", "auth/email/create-your-own-ui"],
        },
        {
          type: "category",
          label: "Social Auth",
          collapsed: true,
          items: [
            "auth/social-auth/overview",
            "auth/social-auth/github",
            "auth/social-auth/google",
            "auth/social-auth/keycloak",
            "auth/social-auth/slack",
            "auth/social-auth/discord",
            "auth/social-auth/microsoft",
            "auth/social-auth/create-your-own-ui",
          ],
        },
        "auth/entities/entities",
        "auth/auth-hooks",
        {
          type: "category",
          label: "Advanced",
          collapsed: true,
          items: ["auth/advanced/custom-auth-actions"],
        },
      ],
    },
    {
      type: "category",
      label: "Project Setup",
      collapsed: false,
      collapsible: true,
      items: [
        "project/starter-templates",
        "project/customizing-app",
        "project/client-config",
        "project/server-config",
        "project/static-assets",
        "project/env-vars",
        "project/testing",
        "project/dependencies",
        "project/custom-vite-config",
      ],
    },
    {
      type: "category",
      label: "Deployment",
      collapsed: false,
      collapsible: true,
      items: [
        "deployment/intro",
        "deployment/env-vars",
        "deployment/database",
        "deployment/local-testing",
        {
          type: "category",
          label: "Deployment Methods",
          collapsed: true,
          items: [
            "deployment/deployment-methods/overview",
            {
              type: "category",
              label: "Wasp Deploy",
              collapsed: true,
              items: [
                "deployment/deployment-methods/wasp-deploy/overview",
                "deployment/deployment-methods/wasp-deploy/fly",
                "deployment/deployment-methods/wasp-deploy/railway",
                "deployment/deployment-methods/wasp-deploy/ci-cd",
              ],
            },
            "deployment/deployment-methods/cloud-providers",
            "deployment/deployment-methods/self-hosted",
          ],
        },
        "deployment/ci-cd",
        "deployment/extras",
      ],
    },
    {
      type: "category",
      label: "AI & Coding Agents",
      collapsed: false,
      collapsible: true,
      items: ["wasp-ai/coding-agent-plugin", "wasp-ai/git-worktrees"],
    },
    {
      type: "category",
      label: "Advanced Features",
      collapsed: false,
      collapsible: true,
      items: [
        "advanced/email",
        "advanced/jobs",
        "advanced/web-sockets",
        "advanced/accessing-app-config",
        "advanced/apis",
        "advanced/middleware-config",
        "advanced/links",
        "advanced/routing",
        "advanced/prerendering",
        "advanced/seo",
      ],
    },
    {
      type: "category",
      label: "General",
      collapsed: false,
      collapsible: true,
      items: ["general/spec", "general/cli", "general/typescript"],
    },
    {
      type: "link",
      label: "Roadmap",
      href: "https://github.com/orgs/wasp-lang/projects/5",
    },
    {
      type: "category",
      label: "Migration guides",
      collapsed: true,
      collapsible: true,
      items: [
        { type: "doc", id: "migration-guide" },
        ...generateMigrationGuideLinks("0.11", [
          "0.12",
          "0.13",
          "0.14",
          "0.15",
          "0.16",
          "0.17",
          "0.18",
          "0.19",
          "0.20",
          "0.21",
          "0.22",
          "0.23",
          "0.24",
          "0.25",
        ]),
      ],
    },
    {
      type: "category",
      label: "Miscellaneous",
      collapsed: true,
      collapsible: true,
      items: ["contributing", "telemetry", "vision", "contact"],
    },
  ],
  guides: [
    {
      type: "autogenerated",
      dirName: "guides",
    },
  ],
  api: [
    {
      type: "category",
      label: "packages",
      collapsed: false,
      collapsible: false,
      items: typedocSidebar.items,
    },
  ],
};

export default sidebars;

function generateMigrationGuideLinks(
  earliestUndocumentedVersion: string,
  docsVersions: string[],
): SidebarItemConfig[] {
  return docsVersions
    .map((currentVersion, index, arr): SidebarItemLink => {
      const prevVersion =
        index == 0 ? earliestUndocumentedVersion : arr[index - 1];

      return {
        type: "link",
        label: `From ${prevVersion} to ${currentVersion}`,
        href: `/docs/${currentVersion}/migration-guide`,
      };
    })
    .reverse();
}
