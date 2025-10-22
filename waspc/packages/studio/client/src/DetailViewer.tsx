import { Card, CardBody, CardHeader, Chip, Divider, Button, Spinner } from "@nextui-org/react";
import { Node } from "reactflow";
import { Data } from "./types";
import { useState } from "react";

interface DetailViewerProps {
  selectedNode: Node | null;
  data: Data;
  onNodeClick: (nodeId: string) => void;
}

interface Relationship {
  id: string;
  name: string;
  type: string;
  direction: "uses" | "usedBy";
}

export function DetailViewer({
  selectedNode,
  data,
  onNodeClick,
}: DetailViewerProps) {
  if (!selectedNode) {
    return (
      <div className="flex h-full items-center justify-center p-8">
        <div className="text-center">
          <div className="mb-4 text-6xl opacity-20">👈</div>
          <h3 className="mb-2 text-lg font-semibold text-gray-400">
            No Selection
          </h3>
          <p className="text-sm text-gray-500">
            Click any node in the graph to view detailed information
          </p>
        </div>
      </div>
    );
  }

  const nodeType = selectedNode.type?.replace("Node", "") || "Unknown";
  const relationships = getRelationships(selectedNode, data);

  return (
    <div className="h-full overflow-y-auto">
      {/* Header Section */}
      <Card className="m-4">
        <CardHeader className="flex-col items-start gap-2 pb-2">
          <Chip color="primary" variant="flat" size="sm">
            {nodeType}
          </Chip>
          <h2 className="text-xl font-bold">
            {selectedNode.data.label || selectedNode.data.name}
          </h2>
        </CardHeader>
        <CardBody className="pt-0">
          <div className="space-y-2 text-sm">
            <InfoRow
              label="Type"
              value={nodeType}
              icon="🏷️"
            />
            <InfoRow
              label="ID"
              value={selectedNode.id}
              icon="🔑"
              mono
            />
            {/* Placeholder for file location */}
            <InfoRow
              label="Location"
              value="src/server/operations.ts:42"
              icon="📁"
              placeholder
              mono
            />
          </div>
        </CardBody>
      </Card>

      {/* AI Summary Section */}
      <AISummaryCard selectedNode={selectedNode} data={data} nodeType={nodeType} />

      {/* Type-Specific Details */}
      {renderTypeSpecificDetails(selectedNode, data, nodeType)}

      {/* Relationships Section */}
      <Card className="m-4">
        <CardHeader className="pb-2">
          <h3 className="text-md font-semibold">Relationships</h3>
        </CardHeader>
        <CardBody className="gap-4 pt-2">
          {/* Dependencies (Uses) */}
          {relationships.uses.length > 0 && (
            <div>
              <h4 className="mb-2 flex items-center gap-2 text-sm font-semibold text-gray-400">
                <span>⬇️</span> Uses ({relationships.uses.length})
              </h4>
              <div className="flex flex-wrap gap-2">
                {relationships.uses.map((rel) => (
                  <RelationshipChip
                    key={rel.id}
                    relationship={rel}
                    onClick={() => onNodeClick(rel.id)}
                  />
                ))}
              </div>
            </div>
          )}

          {/* Dependents (Used By) */}
          {relationships.usedBy.length > 0 && (
            <div>
              <h4 className="mb-2 flex items-center gap-2 text-sm font-semibold text-gray-400">
                <span>⬆️</span> Used By ({relationships.usedBy.length})
              </h4>
              <div className="flex flex-wrap gap-2">
                {relationships.usedBy.map((rel) => (
                  <RelationshipChip
                    key={rel.id}
                    relationship={rel}
                    onClick={() => onNodeClick(rel.id)}
                  />
                ))}
              </div>
            </div>
          )}

          {/* Impact Summary */}
          <div className="mt-2 rounded-lg bg-gray-800 p-3">
            <p className="text-xs text-gray-400">
              📊 Impact:{" "}
              <strong className="text-white">
                {relationships.usedBy.length} dependent
                {relationships.usedBy.length !== 1 ? "s" : ""}
              </strong>
              {relationships.uses.length > 0 && (
                <>
                  {" • "}
                  <strong className="text-white">
                    {relationships.uses.length} dependenc
                    {relationships.uses.length !== 1 ? "ies" : "y"}
                  </strong>
                </>
              )}
            </p>
          </div>
        </CardBody>
      </Card>
    </div>
  );
}

// AI Summary Card Component
function AISummaryCard({
  selectedNode,
  data,
  nodeType,
}: {
  selectedNode: Node;
  data: Data;
  nodeType: string;
}) {
  const [summary, setSummary] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const generateSummary = async () => {
    setLoading(true);
    setError(null);

    try {
      // Prepare context for Claude
      const context = prepareNodeContext(selectedNode, data, nodeType);

      // TODO: Replace with actual backend API endpoint
      // For now, show a placeholder response
      const response = await fetchAISummary(context);
      
      setSummary(response);
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to generate summary");
    } finally {
      setLoading(false);
    }
  };

  return (
    <Card className="m-4 border-2 border-purple-500/30">
      <CardHeader className="flex items-center justify-between pb-2">
        <div className="flex items-center gap-2">
          <span className="text-lg">🤖</span>
          <h3 className="text-md font-semibold">AI Summary</h3>
          <Chip size="sm" variant="flat" color="secondary">
            Claude Haiku
          </Chip>
        </div>
        {!summary && !loading && (
          <Button
            size="sm"
            color="secondary"
            variant="flat"
            onPress={generateSummary}
          >
            Generate
          </Button>
        )}
      </CardHeader>
      <CardBody className="pt-2">
        {!summary && !loading && !error && (
          <div className="rounded-lg bg-purple-950/20 p-4 text-center text-sm text-gray-400">
            <p className="mb-2">✨ Get an AI-powered explanation</p>
            <p className="text-xs">
              Claude will analyze this {nodeType.toLowerCase()} and provide
              insights about its purpose, usage, and relationships.
            </p>
          </div>
        )}

        {loading && (
          <div className="flex items-center justify-center gap-3 rounded-lg bg-purple-950/20 p-6">
            <Spinner size="sm" color="secondary" />
            <span className="text-sm text-gray-400">
              Analyzing with Claude Haiku...
            </span>
          </div>
        )}

        {error && (
          <div className="rounded-lg bg-red-950/30 p-4 text-sm">
            <p className="mb-1 font-semibold text-red-400">⚠️ Error</p>
            <p className="text-xs text-red-300">{error}</p>
            <Button
              size="sm"
              color="danger"
              variant="flat"
              className="mt-2"
              onPress={generateSummary}
            >
              Retry
            </Button>
          </div>
        )}

        {summary && (
          <div className="space-y-3">
            <div className="rounded-lg bg-purple-950/20 p-4 text-sm leading-relaxed text-gray-200">
              {summary}
            </div>
            <div className="flex gap-2">
              <Button
                size="sm"
                variant="flat"
                color="secondary"
                onPress={generateSummary}
              >
                ↻ Regenerate
              </Button>
              <Button
                size="sm"
                variant="flat"
                onPress={() => setSummary(null)}
              >
                Clear
              </Button>
            </div>
          </div>
        )}
      </CardBody>
    </Card>
  );
}

// Prepare context for AI analysis
function prepareNodeContext(
  selectedNode: Node,
  data: Data,
  nodeType: string,
): string {
  const relationships = getRelationships(selectedNode, data);
  const nodeName = selectedNode.data.label || selectedNode.data.name;

  let context = `Analyze this ${nodeType} in a Wasp full-stack application:\n\n`;
  context += `Name: ${nodeName}\n`;
  context += `Type: ${nodeType}\n`;
  context += `ID: ${selectedNode.id}\n\n`;

  // Add type-specific context
  switch (nodeType.toLowerCase()) {
    case "entity":
      context += `This is a database entity/model.\n`;
      context += `User Entity: ${selectedNode.data.isUserEntity ? "Yes (used for authentication)" : "No"}\n`;
      break;
    case "query":
      context += `This is a read operation (query).\n`;
      context += `Authentication: ${selectedNode.data.auth ? "Required" : "Public"}\n`;
      break;
    case "action":
      context += `This is a write operation (action).\n`;
      context += `Authentication: ${selectedNode.data.auth ? "Required" : "Public"}\n`;
      break;
    case "api":
      if (selectedNode.data.httpRoute) {
        context += `HTTP Endpoint: ${selectedNode.data.httpRoute.method} ${selectedNode.data.httpRoute.path}\n`;
      }
      break;
    case "route":
      context += `URL Path: ${selectedNode.data.path}\n`;
      context += `Renders Page: ${selectedNode.data.toPage?.name}\n`;
      break;
    case "page":
      context += `Auth Required: ${selectedNode.data.authRequired ? "Yes" : "No"}\n`;
      break;
    case "job":
      if (selectedNode.data.schedule) {
        context += `Schedule: ${selectedNode.data.schedule}\n`;
      }
      break;
  }

  // Add relationship context
  context += `\nRelationships:\n`;
  if (relationships.uses.length > 0) {
    context += `- Uses: ${relationships.uses.map((r) => r.name).join(", ")}\n`;
  }
  if (relationships.usedBy.length > 0) {
    context += `- Used by: ${relationships.usedBy.map((r) => r.name).join(", ")}\n`;
  }
  context += `\nImpact: ${relationships.usedBy.length} dependents, ${relationships.uses.length} dependencies\n`;

  return context;
}

// API call to backend (with fallback placeholder)
async function fetchAISummary(_context: string): Promise<string> {
  // TODO: Replace with actual backend endpoint
  // Example: const response = await fetch('/api/ai-summary', {
  //   method: 'POST',
  //   headers: { 'Content-Type': 'application/json' },
  //   body: JSON.stringify({ context: _context })
  // });
  
  // PLACEHOLDER: Simulate API call with delay
  await new Promise((resolve) => setTimeout(resolve, 1500));

  // PLACEHOLDER: Return mock response
  // This should be replaced with actual Claude Haiku API call
  return `🤖 **AI Summary** (Placeholder)

This is where Claude Haiku's analysis would appear. The actual implementation would:

1. **Send context to backend** - The frontend sends the node context to your Wasp server
2. **Backend calls Claude** - Server makes API request to Anthropic's Claude Haiku API
3. **Return summary** - AI-generated insights are displayed here

**Example summary format:**
- Purpose: What this component does
- Key responsibilities: Main functions
- Relationships: How it connects to other parts
- Best practices: Recommendations
- Potential issues: Things to watch out for

**To enable:**
1. Set up Anthropic API key in backend
2. Create endpoint: \`/api/ai-summary\`
3. Update \`fetchAISummary()\` to call your endpoint
4. Claude Haiku will provide real insights!`;
}

// Type-specific detail sections
function renderTypeSpecificDetails(
  selectedNode: Node,
  data: Data,
  nodeType: string,
) {
  switch (nodeType.toLowerCase()) {
    case "entity":
      return <EntityDetails node={selectedNode} data={data} />;
    case "query":
    case "action":
      return <OperationDetails node={selectedNode} data={data} />;
    case "api":
      return <ApiDetails node={selectedNode} data={data} />;
    case "route":
      return <RouteDetails node={selectedNode} data={data} />;
    case "page":
      return <PageDetails node={selectedNode} data={data} />;
    case "job":
      return <JobDetails node={selectedNode} data={data} />;
    case "app":
      return <AppDetails node={selectedNode} data={data} />;
    default:
      return null;
  }
}

// Entity-specific details
function EntityDetails({ node }: { node: Node; data: Data }) {
  const isUserEntity = node.data.isUserEntity;

  return (
    <Card className="m-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">Entity Schema</h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        {isUserEntity && (
          <Chip color="warning" variant="flat" size="sm">
            👤 User Entity (Auth)
          </Chip>
        )}
        
        {/* Placeholder for Prisma fields */}
        <div>
          <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
            Fields (Prisma Schema)
          </p>
          <div className="space-y-1 rounded-lg bg-gray-900 p-3 font-mono text-xs">
            <FieldRow name="id" type="String" attributes="@id @default(uuid())" placeholder />
            <FieldRow name="createdAt" type="DateTime" attributes="@default(now())" placeholder />
            <FieldRow name="updatedAt" type="DateTime" attributes="@updatedAt" placeholder />
            {isUserEntity && (
              <>
                <FieldRow name="username" type="String" attributes="@unique" placeholder />
                <FieldRow name="password" type="String" placeholder />
              </>
            )}
            <div className="pt-2 text-gray-500">
              + more fields (from Prisma schema)
            </div>
          </div>
        </div>

        {/* Placeholder for relations */}
        <div>
          <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
            Relations
          </p>
          <div className="rounded-lg bg-gray-900 p-3 text-xs text-gray-500">
            Relation data will appear here
          </div>
        </div>
      </CardBody>
    </Card>
  );
}

// Operation (Query/Action) details
function OperationDetails({ node }: { node: Node; data: Data }) {
  const operationType = node.data.type || "operation";
  const authRequired = node.data.auth;

  return (
    <Card className="m-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">
          {operationType === "query" ? "Query" : "Action"} Details
        </h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        {/* Auth info */}
        <InfoRow
          label="Authentication"
          value={authRequired ? "Required" : "Public"}
          icon={authRequired ? "🔒" : "🌐"}
        />

        {/* Placeholder for function signature */}
        <div>
          <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
            Function Signature
          </p>
          <div className="space-y-1 rounded-lg bg-gray-900 p-3 font-mono text-xs">
            <div className="text-purple-400">
              <span className="text-blue-400">function</span>{" "}
              {node.data.name}(
              <div className="ml-4 text-gray-400">
                args: {"{"}
                <div className="ml-4">
                  <span className="text-green-400">// Parameters</span>
                  <br />
                  <span className="opacity-50">
                    param1: string,
                    <br />
                    param2?: number
                  </span>
                </div>
                {"}"}
              </div>
              ): Promise{"<"}ReturnType{">"}
            </div>
          </div>
          <p className="mt-1 text-xs text-gray-500">
            ℹ️ Placeholder - actual signature from TypeScript
          </p>
        </div>

        {/* Placeholder for parameters */}
        <div>
          <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
            Parameters
          </p>
          <div className="space-y-2">
            <ParamRow name="arg1" type="string" required placeholder />
            <ParamRow name="arg2" type="number" required={false} placeholder />
            <p className="text-xs text-gray-500">
              + Additional parameters from code
            </p>
          </div>
        </div>

        {/* Placeholder for return type */}
        <div>
          <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
            Return Type
          </p>
          <div className="rounded-lg bg-gray-900 p-3 font-mono text-xs text-purple-400">
            Promise{"<"}ResultType{">"}
          </div>
          <p className="mt-1 text-xs text-gray-500">
            ℹ️ Placeholder - actual return type from TypeScript
          </p>
        </div>
      </CardBody>
    </Card>
  );
}

// API details
function ApiDetails({ node }: { node: Node; data: Data }) {
  const httpRoute = node.data.httpRoute;

  return (
    <Card className="m-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">API Endpoint</h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        {httpRoute && (
          <>
            <InfoRow
              label="Method"
              value={httpRoute.method || "GET"}
              icon="🔗"
            />
            <InfoRow
              label="Path"
              value={httpRoute.path || "/api/endpoint"}
              icon="🛣️"
              mono
            />
          </>
        )}
        
        <div>
          <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
            Request/Response
          </p>
          <div className="rounded-lg bg-gray-900 p-3 text-xs text-gray-500">
            Request and response types will appear here
          </div>
        </div>
      </CardBody>
    </Card>
  );
}

// Route details
function RouteDetails({ node }: { node: Node; data: Data }) {
  return (
    <Card className="m-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">Route Details</h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        <InfoRow
          label="Path"
          value={node.data.path || "/"}
          icon="🛣️"
          mono
        />
        <InfoRow
          label="Page"
          value={node.data.toPage?.name || "Unknown"}
          icon="📄"
        />
      </CardBody>
    </Card>
  );
}

// Page details
function PageDetails({ node }: { node: Node; data: Data }) {
  return (
    <Card className="m-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">Page Details</h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        <InfoRow
          label="Auth Required"
          value={node.data.authRequired ? "Yes" : "No"}
          icon={node.data.authRequired ? "🔒" : "🌐"}
        />
        
        <div>
          <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
            Component
          </p>
          <div className="rounded-lg bg-gray-900 p-3 font-mono text-xs text-gray-500">
            Component code location will appear here
          </div>
        </div>
      </CardBody>
    </Card>
  );
}

// Job details
function JobDetails({ node }: { node: Node; data: Data }) {
  return (
    <Card className="m-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">Job Details</h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        {node.data.schedule && (
          <InfoRow
            label="Schedule"
            value={node.data.schedule}
            icon="⏰"
            mono
          />
        )}
        
        <div>
          <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
            Execution
          </p>
          <div className="rounded-lg bg-gray-900 p-3 text-xs text-gray-500">
            Job execution details will appear here
          </div>
        </div>
      </CardBody>
    </Card>
  );
}

// App details
function AppDetails({ node, data }: { node: Node; data: Data }) {
  const auth = node.data.auth;
  const db = node.data.db;

  return (
    <Card className="m-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">App Configuration</h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        {db && (
          <InfoRow
            label="Database"
            value={db.system || "Unknown"}
            icon="🗄️"
          />
        )}
        
        {auth && (
          <>
            <div>
              <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
                Authentication
              </p>
              <div className="space-y-2">
                <InfoRow
                  label="User Entity"
                  value={auth.userEntity?.name || "Unknown"}
                  icon="👤"
                />
                <div>
                  <p className="mb-1 text-xs text-gray-400">Methods:</p>
                  <div className="flex flex-wrap gap-1">
                    {auth.methods?.map((method: string) => (
                      <Chip key={method} size="sm" variant="flat">
                        {method}
                      </Chip>
                    ))}
                  </div>
                </div>
              </div>
            </div>
          </>
        )}

        <Divider />

        <div>
          <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
            Statistics
          </p>
          <div className="grid grid-cols-2 gap-2 text-xs">
            <div className="rounded bg-gray-900 p-2">
              <div className="text-gray-400">Entities</div>
              <div className="text-lg font-bold">{data.entities.length}</div>
            </div>
            <div className="rounded bg-gray-900 p-2">
              <div className="text-gray-400">Operations</div>
              <div className="text-lg font-bold">{data.operations.length}</div>
            </div>
            <div className="rounded bg-gray-900 p-2">
              <div className="text-gray-400">Pages</div>
              <div className="text-lg font-bold">{data.pages.length}</div>
            </div>
            <div className="rounded bg-gray-900 p-2">
              <div className="text-gray-400">Routes</div>
              <div className="text-lg font-bold">{data.routes.length}</div>
            </div>
          </div>
        </div>
      </CardBody>
    </Card>
  );
}

// Helper components
function InfoRow({
  label,
  value,
  icon,
  mono = false,
  placeholder = false,
}: {
  label: string;
  value: string;
  icon?: string;
  mono?: boolean;
  placeholder?: boolean;
}) {
  return (
    <div className="flex items-start justify-between gap-2">
      <span className="text-xs text-gray-400">
        {icon && <span className="mr-1">{icon}</span>}
        {label}:
      </span>
      <span
        className={`text-xs ${mono ? "font-mono" : ""} ${
          placeholder ? "italic text-gray-500" : "text-white"
        }`}
      >
        {value}
      </span>
    </div>
  );
}

function FieldRow({
  name,
  type,
  attributes,
  placeholder = false,
}: {
  name: string;
  type: string;
  attributes?: string;
  placeholder?: boolean;
}) {
  return (
    <div className={placeholder ? "opacity-50" : ""}>
      <span className="text-blue-400">{name}</span>{" "}
      <span className="text-purple-400">{type}</span>{" "}
      {attributes && <span className="text-gray-500">{attributes}</span>}
    </div>
  );
}

function ParamRow({
  name,
  type,
  required,
  placeholder = false,
}: {
  name: string;
  type: string;
  required: boolean;
  placeholder?: boolean;
}) {
  return (
    <div
      className={`rounded-lg bg-gray-900 p-2 text-xs ${placeholder ? "opacity-50" : ""}`}
    >
      <div className="font-mono">
        <span className="text-blue-400">{name}</span>
        {!required && <span className="text-gray-500">?</span>}
        <span className="text-gray-500">: </span>
        <span className="text-purple-400">{type}</span>
      </div>
      {required && (
        <div className="mt-1 text-xs text-red-400">* Required</div>
      )}
    </div>
  );
}

function RelationshipChip({
  relationship,
  onClick,
}: {
  relationship: Relationship;
  onClick: () => void;
}) {
  const colorMap: Record<
    string,
    "success" | "warning" | "secondary" | "primary" | "danger"
  > = {
    entity: "success",
    query: "primary",
    action: "warning",
    page: "secondary",
    route: "secondary",
    api: "danger",
    job: "danger",
  };

  return (
    <Chip
      color={colorMap[relationship.type] || "default"}
      variant="flat"
      size="sm"
      className="cursor-pointer hover:opacity-80"
      onClick={onClick}
    >
      {relationship.name}
      <span className="ml-1 text-xs opacity-60">({relationship.type})</span>
    </Chip>
  );
}

// Get relationships (same logic as before)
function getRelationships(
  selectedNode: Node,
  data: Data,
): {
  uses: Relationship[];
  usedBy: Relationship[];
} {
  const nodeId = selectedNode.id;
  const [nodeType, nodeName] = nodeId.split(":");

  const uses: Relationship[] = [];
  const usedBy: Relationship[] = [];

  switch (nodeType) {
    case "entity":
      data.operations.forEach((op) => {
        if (op.entities.some((e) => e.name === nodeName)) {
          usedBy.push({
            id: `${op.type}:${op.name}`,
            name: op.name,
            type: op.type,
            direction: "usedBy",
          });
        }
      });

      data.apis.forEach((api) => {
        if (api.entities.some((e) => e.name === nodeName)) {
          usedBy.push({
            id: `api:${api.name}`,
            name: api.name,
            type: "api",
            direction: "usedBy",
          });
        }
      });

      data.jobs.forEach((job) => {
        if (job.entities.some((e) => e.name === nodeName)) {
          usedBy.push({
            id: `job:${job.name}`,
            name: job.name,
            type: "job",
            direction: "usedBy",
          });
        }
      });

      uses.push({
        id: `app:${data.app.name}`,
        name: data.app.name,
        type: "app",
        direction: "uses",
      });
      break;

    case "query":
    case "action":
      const operation = data.operations.find((op) => op.name === nodeName);
      if (operation) {
        operation.entities.forEach((entity) => {
          uses.push({
            id: `entity:${entity.name}`,
            name: entity.name,
            type: "entity",
            direction: "uses",
          });
        });
      }
      break;

    case "api":
      const api = data.apis.find((a) => a.name === nodeName);
      if (api) {
        api.entities.forEach((entity) => {
          uses.push({
            id: `entity:${entity.name}`,
            name: entity.name,
            type: "entity",
            direction: "uses",
          });
        });
      }
      break;

    case "job":
      const job = data.jobs.find((j) => j.name === nodeName);
      if (job) {
        job.entities.forEach((entity) => {
          uses.push({
            id: `entity:${entity.name}`,
            name: entity.name,
            type: "entity",
            direction: "uses",
          });
        });
      }
      break;

    case "route":
      const route = data.routes.find((r) => r.path === nodeName);
      if (route) {
        uses.push({
          id: `app:${data.app.name}`,
          name: data.app.name,
          type: "app",
          direction: "uses",
        });
        uses.push({
          id: `page:${route.toPage.name}`,
          name: route.toPage.name,
          type: "page",
          direction: "uses",
        });
      }
      break;

    case "page":
      data.routes.forEach((route) => {
        if (route.toPage.name === nodeName) {
          usedBy.push({
            id: `route:${route.path}`,
            name: route.path,
            type: "route",
            direction: "usedBy",
          });
        }
      });
      break;

    case "app":
      data.entities.forEach((entity) => {
        usedBy.push({
          id: `entity:${entity.name}`,
          name: entity.name,
          type: "entity",
          direction: "usedBy",
        });
      });
      data.routes.forEach((route) => {
        usedBy.push({
          id: `route:${route.path}`,
          name: route.path,
          type: "route",
          direction: "usedBy",
        });
      });
      break;
  }

  return { uses, usedBy };
}

