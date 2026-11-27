import { McpConfirmAction } from "wasp/client/mcp";
import { Card, CardContent } from "../../components/ui/card";
import { mcpAppearance } from "../../oauth/ui/mcpAppearance";

/** app.mcp.confirmRoute: approve or deny a destructive tool call, like delete_link. */
export function ConfirmActionPage() {
  return (
    <Card className="mx-auto max-w-lg" data-testid="confirm-action">
      <CardContent className="pb-6">
        <McpConfirmAction appearance={mcpAppearance} />
      </CardContent>
    </Card>
  );
}
