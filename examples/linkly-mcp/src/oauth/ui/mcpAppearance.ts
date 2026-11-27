import type { McpCustomizationOptions } from "wasp/client/mcp";

/** Wasp's MCP components in Linkly's theme (the shadcn CSS variables in Main.css). */
export const mcpAppearance: McpCustomizationOptions["appearance"] = {
  colors: {
    brand: "var(--primary)",
    brandAccent: "var(--primary)",
    submitButtonText: "var(--primary-foreground)",
    gray700: "var(--muted-foreground)",
    gray600: "var(--border)",
    gray500: "var(--border)",
    gray400: "var(--muted)",
  },
  fontSizes: { sm: "0.875rem" },
};
