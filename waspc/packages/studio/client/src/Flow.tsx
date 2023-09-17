import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
  useState,
} from "react";
import { Data } from "./types";
import ReactFlow, {
  Background,
  Node,
  Edge,
  useEdgesState,
  useNodesState,
  useReactFlow,
} from "reactflow";

import ELK, { type ElkNode } from "elkjs/lib/elk.bundled.js";

import { PageNode } from "./graph/Page";
import { EntityNode } from "./graph/Entity";
import { ActionNode, QueryNode } from "./graph/Operation";
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
import { AppNode } from "./graph/App";
import { RouteNode } from "./graph/Route";
import { ApiNode } from "./graph/Api";
import { JobNode } from "./graph/Job";

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
      // Hardcode a width and height for elk to use when layouting.
      width: Math.max(150, node.data?.label?.length * 13),
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
            // React Flow expects a position property on the node instead of `x`
            // and `y` fields.
            position: { x: node.x, y: node.y },
          })),

          edges: layoutedGraph.edges,
        };
      })
      .catch(console.error)
  );
};

export default function Flow({ data }: { data: Data }) {
  // const { isOpen, onClose } = useDisclosure();
  const [selectedNode, setSelectedNode] = useState<Node | null>(null);

  const [nodes, setNodes] = useNodesState([]);
  const [edges, setEdges] = useEdgesState([]);
  const { fitView } = useReactFlow();
  const nodeTypes = useMemo(
    () => ({
      pageNode: PageNode,
      // operationNode: OperationNode,
      entityNode: EntityNode,
      queryNode: QueryNode,
      actionNode: ActionNode,
      appNode: AppNode,
      routeNode: RouteNode,
      apiNode: ApiNode,
      jobNode: JobNode,
    }),
    []
  );

  const onLayout = useCallback(() => {
    const initialNodes: Node[] = [
      // ASSUMPTION: The names are of everything is unique.
      createAppNode(
        generateId(data.app.name, "app"),
        data.app.name,
        data.app,
        selectedNode
      ),
      ...data.pages.map((page) =>
        createPageNode(
          generateId(page.name, "page"),
          page.name,
          page,
          selectedNode
        )
      ),
      ...data.operations
        .filter((operation) => operation.type === "query")
        .map((query) =>
          createQueryNode(
            generateId(query.name, "query"),
            query.name,
            query,
            selectedNode
          )
        ),
      ...data.operations
        .filter((operation) => operation.type === "action")
        .map((action) =>
          createActionNode(
            generateId(action.name, "action"),
            action.name,
            action,
            selectedNode
          )
        ),
      ...data.entities.map((entity) =>
        createEntityNode(
          generateId(entity.name, "entity"),
          entity.name,
          entity.name === data.app.auth?.userEntity.name,
          entity,
          selectedNode
        )
      ),
      ...data.routes.map((route) =>
        createRouteNode(
          generateId(route.path, "route"),
          route.path,
          route,
          selectedNode
        )
      ),
      ...data.apis.map((api) =>
        createApiNode(generateId(api.name, "api"), api.name, api, selectedNode)
      ),
      ...data.jobs.map((job) =>
        createJobNode(generateId(job.name, "job"), job.name, job, selectedNode)
      ),
      // {
      //   id: "operations",
      //   type: "group",
      //   data: {
      //     label: "Operations",
      //   },
      //   position: { x: 0, y: 0 },
      // },
    ];

    const initialEdges: Edge[] = [
      ...data.entities.map((entity) =>
        createEdge(
          generateId(entity.name, "entity"),
          generateId(data.app.name, "app"),
          selectedNode
        )
      ),
      ...data.routes.map((route) =>
        createEdge(
          generateId(route.path, "route"),
          generateId(route.toPage.name, "page"),
          selectedNode
        )
      ),
      ...data.operations.flatMap((operation) =>
        operation.entities.map((entity) =>
          // ASSUMPTION: operation.type is either "query" or "action"
          createEdge(
            generateId(operation.name, operation.type),
            generateId(entity.name, "entity"),
            selectedNode
          )
        )
      ),
      ...data.apis.flatMap((api) =>
        api.entities.map((entity) =>
          createEdge(
            generateId(api.name, "api"),
            generateId(entity.name, "entity"),
            selectedNode
          )
        )
      ),
      ...data.jobs.flatMap((job) =>
        job.entities.map((entity) =>
          createEdge(
            generateId(job.name, "job"),
            generateId(entity.name, "entity"),
            selectedNode
          )
        )
      ),
      ...data.routes.map((route) =>
        createEdge(
          generateId(data.app.name, "app"),
          generateId(route.path, "route"),
          selectedNode
        )
      ),
    ];

    const [filteredNodes, filteredEdges] = filterNodesAndEdges(
      initialNodes,
      initialEdges,
      selectedNode
    );

    getLayoutedElements(filteredNodes, filteredEdges).then((result) => {
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

  // const onNodesChange: OnNodesChange = useCallback(
  //   (changes) => setNodes((nds) => applyNodeChanges(changes, nds)),
  //   []
  // );
  // const onEdgesChange: OnEdgesChange = useCallback(
  //   (changes) => setEdges((eds) => applyEdgeChanges(changes, eds)),
  //   []
  // );

  useEffect(() => {
    fitView();
  }, [fitView]);

  return (
    <div style={{ height: "100%" }}>
      <ReactFlow
        nodes={nodes}
        edges={edges}
        fitView
        nodeTypes={nodeTypes}
        onNodeClick={(_event, node) => {
          if (node.id === selectedNode?.id) {
            setSelectedNode(null);
            return;
          }
          setSelectedNode(node);
          // onOpen();
        }}
        elevateNodesOnSelect={true}
      >
        <Background
          style={{
            backgroundColor: `hsl(var(--nextui-background)`,
          }}
          color={`#444`}
        />
        {/* <Controls /> */}
      </ReactFlow>

      {/* <Modal size="lg" isOpen={isOpen} onClose={onClose}>
        <ModalContent>
          {() => (
            <>
              <ModalHeader className="flex flex-col gap-1">
                {selectedNode?.data?.label}
              </ModalHeader>
              <ModalBody>
                <pre>
                  {JSON.stringify(selectedNode?.data, null, 2) || "No data"}
                </pre>
              </ModalBody>
              <ModalFooter>
              </ModalFooter>
            </>
          )}
        </ModalContent>
      </Modal> */}
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
    return 100;
  }
  return 50;
}

function generateId(name: string, type: string): string {
  return `${type}:${name}`;
}

function filterNodesAndEdges(
  nodes: Node[],
  edges: Edge[],
  selectedNode: Node | null
): [Node[], Edge[]] {
  if (!selectedNode) {
    return [nodes, edges];
  }

  // Filter out only the nodes and edges that are reachable from the selected
  // node.

  const reachableNodes = new Set<string>();
  const reachableEdges = new Set<string>();

  const queue = [selectedNode.id];
  while (queue.length > 0) {
    const nodeId = queue.shift()!;
    reachableNodes.add(nodeId);
    edges.forEach((edge) => {
      if (edge.source === nodeId) {
        reachableEdges.add(edge.id);
        queue.push(edge.target);
      }
    });
  }

  const filteredNodes = nodes.filter((node) => reachableNodes.has(node.id));
  const filteredEdges = edges.filter((edge) => reachableEdges.has(edge.id));

  return [filteredNodes, filteredEdges];
}
