import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
  useState,
} from "react";
import ReactFlow, {
  Background,
  Edge,
  Node,
  useEdgesState,
  useNodesState,
  useReactFlow,
} from "reactflow";
import { Data } from "./types";
import { DetailViewer } from "./DetailViewer";

import ELK, { type ElkNode } from "elkjs/lib/elk.bundled.js";

import { ApiNode } from "./graph/Api";
import { AppNode } from "./graph/App";
import { EntityNode } from "./graph/Entity";
import { JobNode } from "./graph/Job";
import { ActionNode, QueryNode } from "./graph/Operation";
import { PageNode } from "./graph/Page";
import { RouteNode } from "./graph/Route";
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

const elk = new ELK();

const getLayoutedElements = (nodes: Node[], edges: Edge[]) => {
  const graph = {
    id: "root",
    // Elk has a *huge* amount of options to configure. To see everything you can
    // tweak check out:
    //
    // - https://www.eclipse.org/elk/reference/algorithms.html
    // - https://www.eclipse.org/elk/reference/options.html
    layoutOptions: {
      // Alternative layout:
      // "elk.spacing.nodeNode": "30.0",
      // "elk.algorithm": "elk.layered",
      // "elk.layered.spacing.nodeNodeBetweenLayers": "100.0",
      // "elk.layered.thoroughness": "7",
      // "elk.direction": "RIGHT",
      // "elk.edgeRouting": "POLYLINE",
      // "elk.aspectRatio": "1.0f",
      "elk.algorithm": "layered",
      "elk.direction": "RIGHT",
      "elk.edgeRouting": "POLYLINE",
      // "elk.hierarchyHandling": "INCLUDE_CHILDREN",
      "elk.layered.crossingMinimization.semiInteractive": true,
    },
    children: nodes.map((node: Node) => ({
      ...node,
      width: getNodeWidth(node),
      height: getNodeHeight(node),
    })),
    edges: edges,
  };

  return (
    elk
      // Hack
      .layout(graph as unknown as ElkNode)
      .then((layoutedGraph) => {
        if (!layoutedGraph.children) {
          return null;
        }
        return {
          nodes: layoutedGraph.children.map((node) => ({
            ...node,
            position: { x: node.x, y: node.y },
          })),

          edges: layoutedGraph.edges,
        };
      })
      .catch(console.error)
  );
};

export default function Flow({ data }: { data: Data }) {
  const [selectedNode, setSelectedNode] = useState<Node | null>(null);
  const [showBreadcrumb, setShowBreadcrumb] = useState<boolean>(true);

  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const { fitView, getNode } = useReactFlow();
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
        generateId(data.app.name, "app"),
        data.app.name,
        data.app,
        selectedNode,
      ),
      ...data.pages.map((page) =>
        createPageNode(
          generateId(page.name, "page"),
          page.name,
          page,
          selectedNode,
        ),
      ),
      ...data.operations
        .filter((operation) => operation.type === "query")
        .map((query) =>
          createQueryNode(
            generateId(query.name, "query"),
            query.name,
            query,
            selectedNode,
          ),
        ),
      ...data.operations
        .filter((operation) => operation.type === "action")
        .map((action) =>
          createActionNode(
            generateId(action.name, "action"),
            action.name,
            action,
            selectedNode,
          ),
        ),
      ...data.entities.map((entity) =>
        createEntityNode(
          generateId(entity.name, "entity"),
          entity.name,
          entity.name === data.app.auth?.userEntity.name,
          entity,
          selectedNode,
        ),
      ),
      ...data.routes.map((route) =>
        createRouteNode(
          generateId(route.path, "route"),
          route.path,
          route,
          selectedNode,
        ),
      ),
      ...data.apis.map((api) =>
        createApiNode(generateId(api.name, "api"), api.name, api, selectedNode),
      ),
      ...data.jobs.map((job) =>
        createJobNode(generateId(job.name, "job"), job.name, job, selectedNode),
      ),
    ];

    const initialEdges: Edge[] = [
      ...data.entities.map((entity) =>
        createEdge(
          generateId(entity.name, "entity"),
          generateId(data.app.name, "app"),
          selectedNode,
        ),
      ),
      ...data.routes.map((route) =>
        createEdge(
          generateId(route.path, "route"),
          generateId(route.toPage.name, "page"),
          selectedNode,
        ),
      ),
      ...data.operations.flatMap((operation) =>
        operation.entities.map((entity) =>
          // ASSUMPTION: operation.type is either "query" or "action"
          createEdge(
            generateId(operation.name, operation.type),
            generateId(entity.name, "entity"),
            selectedNode,
          ),
        ),
      ),
      ...data.apis.flatMap((api) =>
        api.entities.map((entity) =>
          createEdge(
            generateId(api.name, "api"),
            generateId(entity.name, "entity"),
            selectedNode,
          ),
        ),
      ),
      ...data.jobs.flatMap((job) =>
        job.entities.map((entity) =>
          createEdge(
            generateId(job.name, "job"),
            generateId(entity.name, "entity"),
            selectedNode,
          ),
        ),
      ),
      ...data.routes.map((route) =>
        createEdge(
          generateId(data.app.name, "app"),
          generateId(route.path, "route"),
          selectedNode,
        ),
      ),
    ];

    getLayoutedElements(initialNodes, initialEdges).then((result) => {
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
  }, [data, setNodes, setEdges, fitView, selectedNode]);

  // Calculate the initial layout on mount.
  useLayoutEffect(() => {
    onLayout();
  }, [onLayout]);

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

    // If selected node is an operation, also highlight its referenced pages
    if (selectedNode.type === "queryNode" || selectedNode.type === "actionNode") {
      const operation = data.operations.find(
        (op) => op.name === selectedNode.data.name
      );
      if (operation) {
        operation.pages.forEach((page) => {
          connectedNodeIds.add(generateId(page.name, "page"));
        });
      }
    }

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
  }, [selectedNode, setNodes, setEdges, edges, data.operations]);

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
            setSelectedNode={setSelectedNode}
            selectedNode={selectedNode}
            data={data}
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
            cursor: "pointer",
            fontSize: "12px",
            fontWeight: "600",
            display: "flex",
            alignItems: "center",
            gap: "6px",
          }}
          title={showBreadcrumb ? "Hide Details" : "Show Details"}
        >
          {showBreadcrumb ? <SideBarOpenIcon /> : <SideBarCloseIcon />}
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

function getNodeHeight(node: Node) {
  if (node.type === "apiNode") {
    return 100;
  }
  if (node.type === "jobNode" && node.data.schedule) {
    return 100;
  }
  if (node.type === "appNode") {
    const authMethods = node.data.auth?.methods ?? [];
    return 100 + authMethods.length * 50;
  }
  return 50;
}

function getNodeWidth(node: Node) {
  const textCandidates = [
    node.data?.label,
    node.data?.name,
    node.data?.path,
    node.data?.schedule,
    // Auth methods
    ...getAuthMethods(node),
  ]
    .filter(Boolean)
    .map((text) => text.length);

  const longestText = Math.max(...textCandidates);
  const width = Math.max(150, longestText * 10 + 40);
  return width;
}

function generateId(name: string, type: string): string {
  return `${type}:${name}`;
}

function getAuthMethods(node: Node) {
  if (node.type !== "appNode") {
    return [];
  }
  return (
    node.data?.auth?.methods.map((method: string) => `Auth: ${method}`) ?? []
  );
}

function SideBarCloseIcon() {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="24"
      height="24"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="2"
      strokeLinecap="round"
      strokeLinejoin="round"
      className="lucide lucide-panel-left-open-icon lucide-panel-left-open"
    >
      <rect width="18" height="18" x="3" y="3" rx="2" />
      <path d="M9 3v18" />
      <path d="m14 9 3 3-3 3" />
    </svg>
  );
}

function SideBarOpenIcon() {
  return (
    <svg
      xmlns="http://www.w3.org/2000/svg"
      width="24"
      height="24"
      viewBox="0 0 24 24"
      fill="none"
      stroke="currentColor"
      strokeWidth="2"
      strokeLinecap="round"
      strokeLinejoin="round"
      className="lucide lucide-panel-left-close-icon lucide-panel-left-close"
    >
      <rect width="18" height="18" x="3" y="3" rx="2" />
      <path d="M9 3v18" />
      <path d="m16 15-3-3 3-3" />
    </svg>
  );
}