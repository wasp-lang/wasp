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
  createRouteNode
} from "./graph/factories";
import { WaspAppData } from "./waspAppData";

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

  // const handleAddEntity = useCallback(
  //   () => {
  //     const newEntity: GetDeclForType<"Entity"> = { declName: "name", declType: "Entity", declValue: {}};
  //     const entityNode = createEntityNode(
  //       newEntity,
  //       selectedNode,
  //     );
  //     const newEdge = createEdge(newEntity, waspAppData.app, selectedNode);

  //     setNodes(oldNodes => [...oldNodes, entityNode]);
  //     setEdges(oldEges => [...oldEges, newEdge]);
  //   },
  //   [selectedNode, waspAppData.app, setNodes, setEdges],
  // );

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
      createAppNode(
        waspAppData.app,
        selectedNode,
      ),
      ...waspAppData.pages.map((page) => createPageNode(page, selectedNode)),
      ...waspAppData.routes.map((route) => createRouteNode(route, selectedNode)),
      ...waspAppData.entities.map((entity) => createEntityNode(entity, selectedNode)),
      ...waspAppData.queries.map((query) => createQueryNode(query, selectedNode)),
      ...waspAppData.actions.map((action) => createActionNode(action, selectedNode)),
      ...waspAppData.apis.map((api) => createApiNode(api, selectedNode)),
      ...waspAppData.jobs.map((job) => createJobNode(job, selectedNode)),
    ];

    const initialEdges: Edge[] = [];
    // Edges from Entities to App
    waspAppData.entities.forEach((entity) => {
      initialEdges.push(
        createEdge(
          entity,
          waspAppData.app,
          selectedNode,
        ),
      );
    });
    // Edges from Routes to Pages
    waspAppData.routes.forEach((route) => {
      initialEdges.push(
        createEdge(
          route,
          route.declValue.to,
          selectedNode,
        ),
      );
    });
    // Edges from Operations (Query/Action) to Entities
    waspAppData.queries.forEach((query) => {
      query.declValue.entities?.forEach((entity) => {
        initialEdges.push(
          createEdge(
            query,
            entity,
            selectedNode,
          ),
        );
      });
    });
    waspAppData.actions.forEach((action) => {
      action.declValue.entities?.forEach((entity) => {
        initialEdges.push(
          createEdge(
            action,
            entity,
            selectedNode,
          ),
        );
      });
    });
    // Edges from APIs to Entities
    waspAppData.apis.forEach((api) => {
      api.declValue.entities?.forEach((entity) => {
        initialEdges.push(
          createEdge(
            api,
            entity,
            selectedNode,
          ),
        );
      });
    });
    // Edges from Jobs to Entities
    waspAppData.jobs.forEach((job) => {
      job.declValue.entities?.forEach((entity) => {
        initialEdges.push(
          createEdge(
            job,
            entity,
            selectedNode,
          ),
        );
      });
    });
    // Edges from App to Routes
    waspAppData.routes.forEach((route) => {
      initialEdges.push(
        createEdge(
          waspAppData.app,
          route,
          selectedNode,
        ),
      );
    });



    console.log(initialEdges)

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
  }, [waspAppData, selectedNode, fitView, setEdges, setNodes]);

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
      const operation = waspAppData.operations.find(
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
  }, [selectedNode, setNodes, setEdges, edges, waspAppData.operations]);

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
            waspAppData={waspAppData}
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
