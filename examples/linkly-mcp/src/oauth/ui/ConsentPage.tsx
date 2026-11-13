import { McpConsentForm } from "wasp/client/mcp";
import { Card, CardContent } from "../../components/ui/card";
import { mcpAppearance } from "./mcpAppearance";

/**
 * The OAuth authorization endpoint (app.mcp.consentRoute). MCP clients send
 * users here to connect; Wasp's consent form does the rest.
 */
export function ConsentPage() {
  return (
    <Card className="mx-auto max-w-lg" data-testid="consent">
      <CardContent className="pb-6">
        <McpConsentForm
          appName="your Linkly account"
          appearance={mcpAppearance}
        />
      </CardContent>
    </Card>
  );
}
