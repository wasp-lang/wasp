/* eslint-disable @typescript-eslint/no-unused-vars */
import {
  Button,
  Card,
  CardBody,
  CardHeader,
  Chip,
  Divider,
  Input,
  Spinner,
  Tab,
  Tabs,
} from "@nextui-org/react";
import { OpenAI } from "openai";
import { useEffect, useState } from "react";
import { Node } from "reactflow";
import { Route } from "./appSpec";
import { generateNodeId } from "./graph/factories";
import { DeclNode, DeclNodeForDecl } from "./node";
import { WaspAppData } from "./waspAppData";

const openai = new OpenAI({
  apiKey: import.meta.env.VITE_OPENAI_API_KEY,
  dangerouslyAllowBrowser: true, // TODO: Remove this once we have a backend
});

interface DetailViewerProps {
  selectedNode: DeclNode | null;
  waspAppData: WaspAppData;
  onNodeClick: (nodeId: string) => void;
}

interface ChatMessage {
  id: string;
  text: string;
  nodesToSelect: string[];
  isUser: boolean;
}

interface Relationship {
  id: string;
  name: string;
  type: string;
  direction: "uses" | "usedBy";
}

export function DetailViewer({
  selectedNode,
  waspAppData,
  onNodeClick,
}: DetailViewerProps) {
  const [messages, setMessages] = useState<ChatMessage[]>([]);
  const [inputValue, setInputValue] = useState("");
  const [isChatLoading, setIsChatLoading] = useState(false);
  const relationships = selectedNode ? getRelationships(selectedNode, waspAppData) : { uses: [], usedBy: [] };

  const handleSendMessage = async () => {
    if (!inputValue.trim()) return;

    setIsChatLoading(true);
    const userMessage: ChatMessage = {
      id: `msg-${Date.now()}`,
      text: inputValue,
      nodesToSelect: [],
      isUser: true,
    };

    setMessages([...messages, userMessage]);
    setInputValue("");

    try {
      // Define the select_node tool
      const tools: OpenAI.Chat.Completions.ChatCompletionTool[] = [
        {
          type: "function",
          function: {
            name: "select_node",
            description:
              "Select and highlight a node in the graph to show its details. Use this when the user asks about a specific entity, operation, page, or other node.",
            parameters: {
              type: "object",
              properties: {
                nodeId: {
                  type: "string",
                  description:
                    "The node ID in format '{type}:{name}' (e.g., 'entity:User', 'query:getGptResponses', 'page:DemoAppPage')",
                },
              },
              required: ["nodeId"],
            },
          },
        },
      ];

      // Create the messages array for the API call
      const apiMessages: OpenAI.Chat.Completions.ChatCompletionMessageParam[] =
        [
          {
            role: "system",
            content: `You're a helpful assistant that can answer questions about the Wasp app structure.

App structure: ${JSON.stringify(waspAppData)}

Available node types and their IDs:
- Entities: ${waspAppData.entities
                .map((entity) => `entity:${entity.declName}`)
                .join(", ")}
- Queries: ${waspAppData.queries
                .map((query) => `query:${query.declName}`)
                .join(", ")}
- Actions: ${waspAppData.actions
                .map((action) => `action:${action.declName}`)
                .join(", ")}
- Pages: ${waspAppData.pages
                .map((page) => `page:${page.declName}`)
                .join(", ")}
- Routes: ${waspAppData.routes
                .map((route) => `route:${route.declValue.path}`)
                .join(", ")}
- APIs: ${waspAppData.apis
                .map((api) => `api:${api.declName}`)
                .join(", ")}
- Jobs: ${waspAppData.jobs
                .map((job) => `job:${job.declName}`)
                .join(", ")}

Use the select_node tool when you want to highlight a specific node in the graph for the user.`,
          },
          {
            role: "user",
            content: inputValue,
          },
        ];

      // Make the initial API call
      let response = await openai.chat.completions.create({
        model: "gpt-4o-mini",
        messages: apiMessages,
        tools: tools,
        tool_choice: "auto",
      });

      const responseMessage = response.choices[0].message;
      const nodesToSelect: string[] = [];

      // Check if the model wants to call tools
      if (responseMessage.tool_calls) {
        // Add the assistant's response to messages
        apiMessages.push(responseMessage);

        // Process each tool call
        for (const toolCall of responseMessage.tool_calls) {
          if (
            toolCall.type === "function" &&
            toolCall.function.name === "select_node"
          ) {
            const args = JSON.parse(toolCall.function.arguments);
            const nodeId = args.nodeId;

            // Execute the tool - select the node
            nodesToSelect.push(nodeId);

            // Add tool result to messages
            apiMessages.push({
              role: "tool",
              tool_call_id: toolCall.id,
              content: JSON.stringify({ success: true, nodeId }),
            });
          }
        }

        // Get the final response from the model
        response = await openai.chat.completions.create({
          model: "gpt-4o-mini",
          messages: apiMessages,
        });
      }

      // Create the AI message
      const finalContent =
        response.choices[0].message.content ||
        "I couldn't generate a response.";
      const aiMessage: ChatMessage = {
        id: `msg-${Date.now()}-ai`,
        text: finalContent,
        nodesToSelect: nodesToSelect,
        isUser: false,
      };

      setMessages((prev) => [...prev, aiMessage]);

      // Select the first node if any were suggested
      if (nodesToSelect.length > 0) {
        onNodeClick(nodesToSelect[0]);
      }
    } catch (error) {
      console.error("Error calling OpenAI:", error);
      const errorMessage: ChatMessage = {
        id: `msg-${Date.now()}-error`,
        text: "Sorry, I encountered an error processing your request.",
        nodesToSelect: [],
        isUser: false,
      };
      setMessages((prev) => [...prev, errorMessage]);
    } finally {
      setIsChatLoading(false);
    }
  };

  const handleKeyPress = (e: React.KeyboardEvent) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      handleSendMessage();
    }
  };

  return (
    <div className="flex h-full flex-col">
      <Tabs aria-label="Detail Viewer Tabs" className="px-4 pt-4">
        <Tab key="chat" title="Chat">
          <div className="flex h-full flex-col">
            {/* Chat Messages Area */}
            <div className="flex-1 overflow-y-auto p-4">
              {messages.length === 0 ? (
                <div className="flex h-full items-center justify-center">
                  <div className="text-center">
                    <div className="mb-4 text-6xl opacity-20">💬</div>
                    <h3 className="mb-2 text-lg font-semibold text-gray-400">
                      Start a Conversation
                    </h3>
                    <p className="text-sm text-gray-500">
                      Ask questions about this {selectedNode?.data.type} or your
                      Wasp app
                    </p>
                  </div>
                </div>
              ) : (
                <div className="space-y-4">
                  {messages.map((message) => (
                    <ChatMessageBubble
                      key={message.id}
                      message={message}
                      onNodeClick={onNodeClick}
                    />
                  ))}
                  {isChatLoading && (
                    <div className="flex items-center justify-center gap-3 rounded-lg bg-purple-950/20 p-6">
                      <Spinner size="sm" color="secondary" />
                      <span className="text-sm text-gray-400">Thinking...</span>
                    </div>
                  )}
                </div>
              )}
            </div>

            {/* Chat Input Area */}
            <div className="border-t border-gray-700 p-4">
              <div className="flex gap-2">
                <Input
                  placeholder="Ask about this node or your app..."
                  value={inputValue}
                  onChange={(e) => setInputValue(e.target.value)}
                  onKeyPress={handleKeyPress}
                  size="sm"
                  className="flex-1"
                />
                <Button
                  color="primary"
                  size="sm"
                  onPress={handleSendMessage}
                  isDisabled={!inputValue.trim()}
                >
                  Send
                </Button>
              </div>
              <p className="mt-2 text-xs text-gray-500">
                Press Enter to send, Shift+Enter for new line
              </p>
            </div>
          </div>
        </Tab>
        <Tab key="details" title="Details">
          {!selectedNode ? (
            <div className="flex h-full items-center justify-center p-8">
              <div className="text-center">
                <div className="mb-4 text-6xl opacity-20">👉</div>
                <h3 className="mb-2 text-lg font-semibold text-gray-400">
                  No Selection
                </h3>
                <p className="text-sm text-gray-500">
                  Click any node in the graph to view detailed information
                </p>
              </div>
            </div>
          ) : (
            <div className="h-full overflow-y-auto pb-4">
              {/* Header Section */}
              <Card className="mb-4">
                <CardHeader className="flex-col items-start gap-2 pb-2">
                  <Chip color="primary" variant="flat" size="sm">
                    {selectedNode?.data.type}
                  </Chip>
                  <h2 className="text-xl font-bold">
                    {selectedNode.data.name}
                  </h2>
                </CardHeader>
                <CardBody className="pt-0">
                  <div className="space-y-2 text-sm">
                    <InfoRow label="Type" value={selectedNode?.data.type} icon="🏷️" />
                    <InfoRow
                      label="ID"
                      value={selectedNode.id}
                      icon="🔑"
                      mono
                    />
                    {(selectedNode.type === 'Action' || selectedNode.type === 'Query') && <InfoRow
                      label="Location"
                      value={`${selectedNode.data.value.fn.name}@${selectedNode.data.value.fn.path}`}
                      icon="📁"
                      mono
                    />}
                    {(selectedNode.type === 'Page') && <InfoRow
                      label="Location"
                      value={selectedNode.data.value.component.path}
                      icon="📁"
                      mono
                    />}
                  </div>
                </CardBody>
              </Card>
              {/* AI Summary Section */}
              <AISummaryCard
                selectedNode={selectedNode}
                waspAppData={waspAppData}
              />

              {/* Type-Specific Details */}
              {renderTypeSpecificDetails(selectedNode, waspAppData)}

              {/* Relationships Section */}
              <Card className="mb-4">
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
          )}
        </Tab>
      </Tabs>
    </div>
  );
}

function ChatMessageBubble({
  message,
  onNodeClick,
}: {
  message: ChatMessage;
  onNodeClick: (nodeId: string) => void;
}) {
  return (
    <div className={`flex ${message.isUser ? "justify-end" : "justify-start"}`}>
      <div
        className={`max-w-[80%] rounded-lg p-3 ${message.isUser ? "bg-primary text-white" : "bg-gray-800 text-gray-200"
          }`}
      >
        <p className="text-sm">{message.text}</p>
        {message.nodesToSelect.length > 0 && (
          <div className="mt-2 flex flex-wrap gap-1">
            {message.nodesToSelect.map((nodeId) => (
              <Chip
                key={nodeId}
                size="sm"
                variant="flat"
                className="cursor-pointer"
                onClick={() => onNodeClick(nodeId)}
              >
                {nodeId}
              </Chip>
            ))}
          </div>
        )}
      </div>
    </div>
  );
}

function AISummaryCard({
  selectedNode,
  waspAppData,
}: {
  selectedNode: DeclNode;
  waspAppData: WaspAppData;
}) {
  const [summary, setSummary] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const generateSummary = async () => {
    setLoading(true);
    setError(null);

    try {
      // Prepare context for Claude
      const context = prepareNodeContext(selectedNode, waspAppData);

      // TODO: Replace with actual backend API endpoint
      // For now, show a placeholder response
      const response = await fetchAISummary(context);

      setSummary(response);
    } catch (err) {
      setError(
        err instanceof Error ? err.message : "Failed to generate summary",
      );
    } finally {
      setLoading(false);
    }
  };

  return (
    <Card className="mb-4 border-2 border-purple-500/30">
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
              Claude will analyze this {selectedNode.data.type} and provide
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
              <Button size="sm" variant="flat" onPress={() => setSummary(null)}>
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
  selectedNode: DeclNode,
  waspAppData: WaspAppData,
): string {
  const relationships = getRelationships(selectedNode, waspAppData);
  const nodeName = selectedNode.data.name;
  const nodeType = selectedNode.data.type;

  let context = `Analyze this ${nodeType} in a Wasp full-stack application:\n\n`;
  context += `Name: ${nodeName}\n`;
  context += `Type: ${nodeType}\n`;
  context += `ID: ${selectedNode.id}\n\n`;

  // Add type-specific context
  switch (nodeType) {
    case "Entity":
      context += `This is a database entity/model.\n`;
      context += `User Entity: ${selectedNode.data.name === "User" ? "Yes (used for authentication)" : "No"}\n`;
      break;
    case "Query":
      context += `This is a read operation (query).\n`;
      context += `Authentication: ${selectedNode.data.value.auth ? "Required" : "Public"}\n`;
      break;
    case "Action":
      context += `This is a write operation (action).\n`;
      context += `Authentication: ${selectedNode.data.value.auth ? "Required" : "Public"}\n`;
      break;
    case "Api":
      if (selectedNode.data.value.httpRoute) {
        context += `HTTP Endpoint: ${selectedNode.data.value.httpRoute[0]} ${selectedNode.data.value.httpRoute[1]}\n`;
      }
      break;
    case "Route":
      context += `URL Path: ${selectedNode.data.value.path}\n`;
      context += `Renders Page: ${selectedNode.data.value.to.name}\n`;
      break;
    case "Page":
      context += `Auth Required: ${selectedNode.data.value.authRequired ? "Yes" : "No"}\n`;
      break;
    case "Job":
      if (selectedNode.data.value.schedule) {
        context += `Schedule: ${selectedNode.data.value.schedule}\n`;
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
  selectedNode: DeclNode,
  waspAppData: WaspAppData,
) {
  switch (selectedNode.data.type) {
    case "Entity":
      return <EntityDetails node={selectedNode as DeclNodeForDecl<"Entity">} waspAppData={waspAppData} />;
    case "Query":
    case "Action":
      return <OperationDetails node={selectedNode} waspAppData={waspAppData} />;
    case "Api":
      return <ApiDetails node={selectedNode} waspAppData={waspAppData} />;
    case "Route":
      return <RouteDetails node={selectedNode} waspAppData={waspAppData} />;
    case "Page":
      return <PageDetails node={selectedNode} waspAppData={waspAppData} />;
    case "Job":
      return <JobDetails node={selectedNode} waspAppData={waspAppData} />;
    case "App":
      return <AppDetails node={selectedNode} waspAppData={waspAppData} />;
    default:
      return null;
  }
}

// Entity-specific details
function EntityDetails({ node, waspAppData }: { node: DeclNodeForDecl<"Entity">; waspAppData: WaspAppData }) {
  const isUserEntity = node.data.name === "User";
  const prismaSchemaPath = [waspAppData.waspProjectDir, "schema.prisma"].join("/");
  const [modelDefinition, setModelDefinition] = useState<string | null>(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    async function fetchPrismaModel() {
      try {
        const response = await fetch(
          `http://localhost:4000/api/prisma-schema?path=${encodeURIComponent(prismaSchemaPath)}`
        );
        if (!response.ok) {
          throw new Error("Failed to fetch Prisma schema");
        }
        const data = await response.json();
        const schemaContent = data.content as string;
        
        // Extract the model definition for this entity
        const modelName = node.data.name;
        const modelRegex = new RegExp(
          `model\\s+${modelName}\\s*\\{[\\s\\S]*?\\n\\}`,
          "m"
        );
        const match = schemaContent.match(modelRegex);
        
        if (match) {
          setModelDefinition(match[0]);
        } else {
          setModelDefinition(null);
        }
      } catch (error) {
        console.error("Error fetching Prisma schema:", error);
        setModelDefinition(null);
      } finally {
        setIsLoading(false);
      }
    }

    fetchPrismaModel();
  }, [node.data.name, prismaSchemaPath]);

  return (
    <Card className="mb-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">Entity Schema</h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        {isUserEntity && (
          <Chip color="warning" variant="flat" size="sm">
            👤 User Entity (Auth)
          </Chip>
        )}

        {/* Prisma Schema Definition */}
        <div>
          <p className="mb-2 text-xs font-semibold uppercase text-gray-400">
            Prisma Schema
          </p>
          {isLoading ? (
            <div className="flex items-center justify-center rounded-lg bg-gray-900 p-3">
              <Spinner size="sm" />
            </div>
          ) : modelDefinition ? (
            <div className="rounded-lg bg-gray-900 p-3 font-mono text-xs">
              <pre className="whitespace-pre-wrap text-gray-300">
                {modelDefinition}
              </pre>
            </div>
          ) : (
            <div className="rounded-lg bg-gray-900 p-3 text-xs text-gray-500">
              Model definition not found in schema.prisma
            </div>
          )}
        </div>
      </CardBody>
    </Card>
  );
}

// Operation (Query/Action) details
function OperationDetails({ node }: { node: Node; waspAppData: WaspAppData }) {
  const operationType = node.data.type || "operation";
  const authRequired = node.data.auth;

  return (
    <Card className="mb-4">
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
              <span className="text-blue-400">function</span> {node.data.name}(
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
function ApiDetails({ node }: { node: Node; waspAppData: WaspAppData }) {
  const httpRoute = node.data.httpRoute;

  return (
    <Card className="mb-4">
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
function RouteDetails({ node }: { node: Node; waspAppData: WaspAppData }) {
  return (
    <Card className="mb-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">Route Details</h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        <InfoRow label="Path" value={node.data.path || "/"} icon="🛣️" mono />
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
function PageDetails({ node }: { node: Node; waspAppData: WaspAppData }) {
  return (
    <Card className="mb-4">
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
function JobDetails({ node }: { node: Node; waspAppData: WaspAppData }) {
  return (
    <Card className="mb-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">Job Details</h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        {node.data.schedule && (
          <InfoRow label="Schedule" value={node.data.schedule} icon="⏰" mono />
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
function AppDetails({ node, waspAppData }: { node: Node; waspAppData: WaspAppData }) {
  const auth = node.data.auth;
  const db = node.data.db;

  return (
    <Card className="mb-4">
      <CardHeader className="pb-2">
        <h3 className="text-md font-semibold">App Configuration</h3>
      </CardHeader>
      <CardBody className="gap-3 pt-2">
        {db && (
          <InfoRow label="Database" value={db.system || "Unknown"} icon="🗄️" />
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
              <div className="text-lg font-bold">{waspAppData.entities.length}</div>
            </div>
            <div className="rounded bg-gray-900 p-2">
              <div className="text-gray-400">Queries</div>
              <div className="text-lg font-bold">{waspAppData.queries.length}</div>
            </div>
            <div className="rounded bg-gray-900 p-2">
              <div className="text-gray-400">Actions</div>
              <div className="text-lg font-bold">{waspAppData.actions.length}</div>
            </div>
            <div className="rounded bg-gray-900 p-2">
              <div className="text-gray-400">Pages</div>
              <div className="text-lg font-bold">{waspAppData.pages.length}</div>
            </div>
            <div className="rounded bg-gray-900 p-2">
              <div className="text-gray-400">Routes</div>
              <div className="text-lg font-bold">{waspAppData.routes.length}</div>
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
        className={`text-xs ${mono ? "font-mono" : ""} ${placeholder ? "italic text-gray-500" : "text-white"
          }`}
      >
        {value}
      </span>
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
      {required && <div className="mt-1 text-xs text-red-400">* Required</div>}
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
  selectedNode: DeclNode,
  waspAppData: WaspAppData,
): {
  uses: Relationship[];
  usedBy: Relationship[];
} {
  const uses: Relationship[] = [];
  const usedBy: Relationship[] = [];

  switch (selectedNode.data.type) {
    case "Entity":
      waspAppData.actions.forEach((action) => {
        if (action.declValue.entities?.some((entity) => entity.name === selectedNode.data.name)) {
          usedBy.push({
            id: generateNodeId(action),
            name: action.declName,
            type: action.declType,
            direction: "usedBy",
          });
        }
      });
      waspAppData.queries.forEach((query) => {
        if (query.declValue.entities?.some((entity) => entity.name === selectedNode.data.name)) {
          usedBy.push({
            id: generateNodeId(query),
            name: query.declName,
            type: query.declType,
            direction: "usedBy",
          });
        }
      });
      waspAppData.apis.forEach((api) => {
        if (api.declValue.entities?.some((entity) => entity.name === selectedNode.data.name)) {
          usedBy.push({
            id: generateNodeId(api),
            name: api.declName,
            type: api.declType,
            direction: "usedBy",
          });
        }
      });
      waspAppData.jobs.forEach((job) => {
        if (job.declValue.entities?.some((entity) => entity.name === selectedNode.data.name)) {
          usedBy.push({
            id: generateNodeId(job),
            name: job.declName,
            type: job.declType,
            direction: "usedBy",
          });
        }
      });
      uses.push({
        id: generateNodeId(waspAppData.app),
        name: waspAppData.app.declName,
        type: waspAppData.app.declType,
        direction: "uses",
      });
      break;

    case "Query":
      waspAppData.queries
        .find((query) => query.declName === selectedNode.data.name)
        ?.declValue.entities
        ?.forEach((entity) => {
          uses.push({
            id: generateNodeId(entity),
            name: entity.name,
            type: entity.declType,
            direction: "uses",
          });
        });
      break;

    case "Action":
      waspAppData.actions
        .find((action) => action.declName === selectedNode.data.name)
        ?.declValue.entities
        ?.forEach((entity) => {
          uses.push({
            id: generateNodeId(entity),
            name: entity.name,
            type: entity.declType,
            direction: "uses",
          });
        });
      break;
      
    case "Api":
      waspAppData.apis
        .find((api) => api.declName === selectedNode.data.name)
        ?.declValue.entities
        ?.forEach((entity) => {
          uses.push({
            id: generateNodeId(entity),
            name: entity.name,
            type: entity.declType,
            direction: "uses",
          });
        });
      break;

    case "Job":
      waspAppData.jobs
        .find((job) => job.declName === selectedNode.data.name)
        ?.declValue.entities
        ?.forEach((entity) => {
          uses.push({
            id: generateNodeId(entity),
            name: entity.name,
            type: entity.declType,
            direction: "uses",
          });
        });
      break;

    case "Route":
      waspAppData.pages.forEach((page) => {
        if (page.declName === (selectedNode.data.value as Route).to.name) {
          usedBy.push({
            id: generateNodeId(page),
            name: page.declName,
            type: page.declType,
            direction: "usedBy",
          });
        }
      });
      waspAppData.pages.forEach((page) => {
        if (page.declName === (selectedNode.data.value as Route).to.name) {
          uses.push({
            id: generateNodeId(waspAppData.app),
            name: waspAppData.app.declName,
            type: waspAppData.app.declType,
            direction: "uses",
          });
        }
      });
      break;

    case "Page":
      waspAppData.routes.forEach((route) => {
        if (route.declValue.to.name === selectedNode.data.name) {
          usedBy.push({
            id: generateNodeId(route),
            name: route.declName,
            type: route.declType,
            direction: "usedBy",
          });
        }
      });
      break;

    case "App":
      waspAppData.entities.forEach((entity) => {
        usedBy.push({
          id: generateNodeId(entity),
          name: entity.declName,
          type: entity.declType,
          direction: "usedBy",
        });
      });
      waspAppData.routes.forEach((route) => {
        usedBy.push({
          id: generateNodeId(route),
          name: route.declName,
          type: route.declType,
          direction: "usedBy",
        });
      });
      break;
  }

  return { uses, usedBy };
}
