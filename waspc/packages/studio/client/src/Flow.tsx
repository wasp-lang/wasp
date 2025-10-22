import {
  useCallback,
  useEffect,
  useMemo,
  useState
} from "react";
import ReactFlow, {
  Background,
  Edge,
  Node,
  useEdgesState,
  useNodesState,
  useReactFlow,
} from "reactflow";
import { DetailViewer } from "./DetailViewer";
import { getLayoutedElements } from "./ELK";
import { ApiNode } from "./graph/ApiNode";
import { AppNode } from "./graph/AppNode";
import { EntityNode } from "./graph/EntityNode";
import { JobNode } from "./graph/JobNode";
import { ActionNode, QueryNode } from "./graph/OperationNode";
import { PageNode } from "./graph/PageNode";
import { RouteNode } from "./graph/RouteNode";
import {
  createActionNode,
  createApiNode,
  createAppNode,
  createEdge,
  createEntityNode,
  createJobNode,
  createPageNode,
  createQueryNode,
  createRouteNode,
} from "./graph/factories";
import { WaspAppData } from "./types";

interface FlowProps {
  waspAppData: WaspAppData;
}

export default function Flow({
  waspAppData,
}: FlowProps) {
  // NOTE: This is not used. But it might be useful in the future.
  const [selectedNode, setSelectedNode] = useState<Node | null>(null);
  const [showBreadcrumb, setShowBreadcrumb] = useState<boolean>(true);
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const { fitView, getNode } = useReactFlow();

  const handleAddEntity = useCallback(
    (entityName: string) => {
      const entityNodeId = generateId(entityName, "entity");
      const appNodeId = generateId(waspAppData.app.name, "app");

      const newNode = createEntityNode(
        entityNodeId,
        entityName,
        false,
        { name: entityName },
        selectedNode,
      );
      const newEdge = createEdge(entityNodeId, appNodeId, selectedNode);

      setNodes(oldNodes => [...oldNodes, newNode]);
      setEdges(oldEges => [...oldEges, newEdge]);
    },
    [waspAppData.app.name, selectedNode, setNodes, setEdges],
  );

  const handleAddRoute = useCallback(() => {
    // TODO: Implement logic to add a new route
    console.log("Add route clicked");
  }, []);

  const nodeTypes = useMemo(
    () => ({
      pageNode: PageNode,
      entityNode: EntityNode,
      queryNode: QueryNode,
      actionNode: ActionNode,
      appNode: AppNode,
      routeNode: RouteNode,
      apiNode: ApiNode,
      jobNode: JobNode,
    }),
    [],
  );

  const onLayout = useCallback(() => {
    const initialNodes: Node[] = [
      // ASSUMPTION: The names are of everything is unique.
      createAppNode(
        generateId(waspAppData.app.name, "app"),
        waspAppData.app.name,
        {
          ...waspAppData.app,
          onAddEntity: handleAddEntity,
          onAddRoute: handleAddRoute,
        },
        selectedNode,
      ),
      ...waspAppData.pages.map((page) =>
        createPageNode(
          generateId(page.name, "page"),
          page.name,
          page,
          selectedNode,
        ),
      ),
      ...waspAppData.operations
        .filter((operation) => operation.type === "query")
        .map((query) =>
          createQueryNode(
            generateId(query.name, "query"),
            query.name,
            query,
            selectedNode,
          ),
        ),
      ...waspAppData.operations
        .filter((operation) => operation.type === "action")
        .map((action) =>
          createActionNode(
            generateId(action.name, "action"),
            action.name,
            action,
            selectedNode,
          ),
        ),
      ...waspAppData.entities.map((entity) =>
        createEntityNode(
          generateId(entity.name, "entity"),
          entity.name,
          entity.name === waspAppData.app.auth?.userEntity.name,
          entity,
          selectedNode,
        ),
      ),
      ...waspAppData.routes.map((route) =>
        createRouteNode(
          generateId(route.path, "route"),
          route.path,
          route,
          selectedNode,
        ),
      ),
      ...waspAppData.apis.map((api) =>
        createApiNode(generateId(api.name, "api"), api.name, api, selectedNode),
      ),
      ...waspAppData.jobs.map((job) =>
        createJobNode(generateId(job.name, "job"), job.name, job, selectedNode),
      ),
    ];

    const initialEdges: Edge[] = [
      ...waspAppData.entities.map((entity) =>
        createEdge(
          generateId(entity.name, "entity"),
          generateId(waspAppData.app.name, "app"),
          selectedNode,
        ),
      ),
      ...waspAppData.routes.map((route) =>
        createEdge(
          generateId(route.path, "route"),
          generateId(route.toPage.name, "page"),
          selectedNode,
        ),
      ),
      ...waspAppData.operations.flatMap((operation) =>
        operation.entities.map((entity) =>
          // ASSUMPTION: operation.type is either "query" or "action"
          createEdge(
            generateId(operation.name, operation.type),
            generateId(entity.name, "entity"),
            selectedNode,
          ),
        ),
      ),
      ...waspAppData.apis.flatMap((api) =>
        api.entities.map((entity) =>
          createEdge(
            generateId(api.name, "api"),
            generateId(entity.name, "entity"),
            selectedNode,
          ),
        ),
      ),
      ...waspAppData.jobs.flatMap((job) =>
        job.entities.map((entity) =>
          createEdge(
            generateId(job.name, "job"),
            generateId(entity.name, "entity"),
            selectedNode,
          ),
        ),
      ),
      ...waspAppData.routes.map((route) =>
        createEdge(
          generateId(waspAppData.app.name, "app"),
          generateId(route.path, "route"),
          selectedNode,
        ),
      ),
    ];

    getLayoutedElements(initialNodes, initialEdges).then((result) => {
      console.log("here")
      if (!result) {
        return;
      }
      const { nodes: layoutedNodes, edges: layoutedEdges } = result;
      if (!layoutedNodes || !layoutedEdges) {
        return;
      }
      // Hack
      setNodes(layoutedNodes as Node[]);
      // Hack
      setEdges(layoutedEdges as unknown as Edge[]);
      window.requestAnimationFrame(() => fitView());
    });
  }, [
    waspAppData,
    handleAddEntity,
    handleAddRoute,
    selectedNode,
    fitView,
    setEdges,
    setNodes
  ]);

  useEffect(() => {
    setTimeout(() => {
      fitView();
    }, 100);
  }, [fitView, selectedNode]);

  useEffect(() => {
    fitView();
  }, [fitView]);

  const handleNodeClick = useCallback(
    (_event: React.MouseEvent, node: Node) => {
      setSelectedNode(node);
    },
    [],
  );

  const handlePaneClick = useCallback(() => {
    setSelectedNode(null);
  }, []);

  const handleNavigateToNode = useCallback(
    (nodeId: string) => {
      const node = getNode(nodeId);
      if (node) {
        setSelectedNode(node);
        // Optionally, center the node in view
        fitView({
          nodes: [node],
          duration: 500,
          padding: 0.5,
        });
      }
    },
    [getNode, fitView],
  );

  // Update node selection state and mark connected nodes
  useEffect(() => {
    if (!selectedNode) {
      // Clear all selections and connected states
      setNodes((nds) =>
        nds.map((node) => ({
          ...node,
          selected: false,
          className: "",
        })),
      );
      setEdges((eds) =>
        eds.map((edge) => ({
          ...edge,
          selected: false,
        })),
      );
      return;
    }

    // Find all nodes connected to the selected node
    const connectedNodeIds = new Set<string>();
    edges.forEach((edge) => {
      if (edge.source === selectedNode.id) {
        connectedNodeIds.add(edge.target);
      }
      if (edge.target === selectedNode.id) {
        connectedNodeIds.add(edge.source);
      }
    });

    setNodes((nds) =>
      nds.map((node) => ({
        ...node,
        selected: node.id === selectedNode.id,
        className: connectedNodeIds.has(node.id) ? "connected" : "",
      })),
    );
    setEdges((eds) =>
      eds.map((edge) => ({
        ...edge,
        selected:
          edge.source === selectedNode.id || edge.target === selectedNode.id,
      })),
    );
  }, [selectedNode, setNodes, setEdges, edges]);

  return (
    <div style={{ height: "100%", display: "flex", position: "relative" }}>
      {/* Detail Viewer Panel */}
      {showBreadcrumb && (
        <div
          style={{
            width: "400px",
            maxHeight: "100%",
            overflowY: "auto",
            borderRight: "1px solid #333",
            background: "hsl(var(--nextui-background))",
          }}
        >
          <DetailViewer
            selectedNode={selectedNode}
            data={waspAppData}
            onNodeClick={handleNavigateToNode}
          />
        </div>
      )}

      {/* Main Graph */}
      <div style={{ flex: 1, position: "relative" }}>
        {/* Toggle Button */}
        <button
          onClick={() => setShowBreadcrumb(!showBreadcrumb)}
          style={{
            position: "absolute",
            top: "10px",
            left: "10px",
            zIndex: 10,
            padding: "8px 12px",
            background: "#333",
            border: "1px solid #555",
            borderRadius: "6px",
            color: "#fff",
            cursor: "pointer",
            fontSize: "12px",
            fontWeight: "600",
            display: "flex",
            alignItems: "center",
            gap: "6px",
          }}
          title={showBreadcrumb ? "Hide Details" : "Show Details"}
        >
          <span>{showBreadcrumb ? "◀" : "▶"}</span>
          <span>{showBreadcrumb ? "Hide" : "Show"} Details</span>
        </button>
        <ReactFlow
          nodes={nodes}
          edges={edges}
          onNodesChange={onNodesChange}
          onEdgesChange={onEdgesChange}
          onNodeClick={handleNodeClick}
          onPaneClick={handlePaneClick}
          fitView
          nodeTypes={nodeTypes}
          className={selectedNode ? "focus-mode" : ""}
          onInit={onLayout}
        >
          <Background
            style={{
              backgroundColor: `hsl(var(--nextui-background)`,
            }}
            color={`#444`}
          />
        </ReactFlow>
      </div>
    </div>
  );
}

function generateId(name: string, type: string): string {
  return `${type}:${name}`;
}
