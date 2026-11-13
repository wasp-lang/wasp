import { McpConnections, mcpServerUrl } from "wasp/client/mcp";
import { CopyField } from "../../app/ui/CopyField";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "../../components/ui/card";
import { mcpAppearance } from "./mcpAppearance";

/** Connected apps: Wasp's connections list, plus a Claude Code command to copy. */
export function ConnectionsPage() {
  return (
    <div className="space-y-6">
      <div className="space-y-1">
        <h1 className="font-heading text-2xl font-semibold">Connections</h1>
        <p className="text-muted-foreground">
          AI assistants you've connected to Linkly. Disconnecting takes effect
          on the app's next request.
        </p>
      </div>
      <Card>
        <CardHeader>
          <CardTitle>Claude Code</CardTitle>
          <CardDescription>
            You approve the connection in your browser, so the agent never gets
            your password.
          </CardDescription>
        </CardHeader>
        <CardContent>
          <CopyField
            label="Command"
            value={`claude mcp add --transport http linkly ${mcpServerUrl}`}
            testId="connect-command"
          />
        </CardContent>
      </Card>
      <McpConnections appearance={mcpAppearance} />
    </div>
  );
}
