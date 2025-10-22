import {
  useCallback,
  useEffect,
  useState
} from "react";
import ReactFlow, {
  Background,
  Edge,
  getConnectedEdges,
  useEdgesState,
  useNodesState,
  useReactFlow
} from "reactflow";
import { DetailViewer } from "./DetailViewer";
import { getLayoutedElements } from "./ELK";
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
import { DeclNode, declNodeTypes } from "./node";
import { WaspAppData } from "./waspAppData";

interface FlowProps {
  waspAppData: WaspAppData;
}

export default function Flow({
  waspAppData,
}: FlowProps) {
  const [selectedNode, setSelectedNode] = useState<DeclNode | null>(null);
  const [showBreadcrumb, setShowBreadcrumb] = useState<boolean>(true);
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const { fitView, getNode } = useReactFlow();

  // NOTE:
  // To make adding/removing entities work,
  // the "waspAppData" must be connected to edges/nodes bidirectionally.

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

  const onLayout = useCallback(() => {
    const initialNodes: DeclNode[] = [
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

    getLayoutedElements(initialNodes, initialEdges).then((result) => {
      if (!result) {
        return;
      }
      const { nodes: layoutedNodes, edges: layoutedEdges } = result;
      if (!layoutedNodes || !layoutedEdges) {
        return;
      }
      // Hack
      setNodes(layoutedNodes as DeclNode[]);
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

  const handleNodeClick = useCallback(
    (_event: React.MouseEvent, node: Node) => {
      setSelectedNode(node as unknown as DeclNode);
    },
    [],
  );

  const handlePaneClick = useCallback(() => {
    setSelectedNode(null);
  }, []);

  const handleNavigateToNode = useCallback(
    (nodeId: string) => {
      const node = getNode(nodeId) as DeclNode | undefined;
      if (node) {
        setSelectedNode(node);
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

    const connectedNodeIds = getConnectedEdges([selectedNode], edges).map(edge => edge.id);
  
    setNodes((nds) =>
      nds.map((node) => ({
        ...node,
        selected: node.id === selectedNode.id,
        className: connectedNodeIds.includes(node.id) ? "connected" : "",
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
            onNodeClick={handleNavigateToNode}
            waspAppData={waspAppData}
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
          nodeTypes={declNodeTypes}
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
