import { Link as RouterLink } from "react-router";
import { config } from "wasp/client";
import { useAuth } from "wasp/client/auth";
import { Button } from "../components/ui/button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "../components/ui/card";
import { CopyField } from "./ui/CopyField";

// Prerendered (logged-out view), so the "For AI agents" card is in the HTML an
// agent gets when it fetches the dashboard's address.
export function HomePage() {
  const { data: user } = useAuth();
  return (
    <div className="mx-auto max-w-2xl space-y-12 py-16">
      <section className="space-y-6 text-center">
        <h1 className="font-heading text-4xl font-semibold tracking-tight">
          Short links, with an MCP server.
        </h1>
        <p className="text-muted-foreground text-lg">
          Linkly shortens links and counts clicks. Everything the dashboard can
          do is also available through an MCP server your AI assistant can
          connect to.
        </p>
        <div className="flex justify-center gap-3">
          {user ? (
            <Button asChild size="lg">
              <RouterLink to="/links">Go to your links</RouterLink>
            </Button>
          ) : (
            <>
              <Button asChild size="lg">
                <RouterLink to="/signup">Get started</RouterLink>
              </Button>
              <Button asChild size="lg" variant="outline">
                <RouterLink to="/login">Log in</RouterLink>
              </Button>
            </>
          )}
        </div>
      </section>
      <ForAgents />
    </div>
  );
}

function ForAgents() {
  return (
    <Card>
      <CardHeader>
        <CardTitle>For AI agents</CardTitle>
        <CardDescription>
          Connect to Linkly's MCP server and log in with OAuth in the user's
          browser: there's no API key to copy and no client registration. Never
          ask the user for their Linkly password.
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-4">
        <CopyField label="MCP server" value={`${config.apiUrl}/mcp`} />
      </CardContent>
    </Card>
  );
}
