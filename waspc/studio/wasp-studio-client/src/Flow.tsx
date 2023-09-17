import {
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
  useState,
} from "react";
import { Data } from "./types";
import {
  Modal,
  ModalBody,
  ModalContent,
  ModalFooter,
  ModalHeader,
  useDisclosure,
} from "@nextui-org/react";
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
  const { isOpen, onOpen, onClose } = useDisclosure();
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
      createAppNode(data.app.name, data.app.name, data.app),
      ...data.pages.map((page) => createPageNode(page.name, page.name, page)),
      ...data.operations
        .filter((operation) => operation.type === "query")
        .map((query) => createQueryNode(query.name, query.name, query)),
      ...data.operations
        .filter((operation) => operation.type === "action")
        .map((action) => createActionNode(action.name, action.name, action)),
      ...data.entities.map((entity) =>
        createEntityNode(
          entity.name,
          entity.name,
          entity.name === data.app.auth?.userEntity.name,
          entity
        )
      ),
      ...data.routes.map((route) =>
        createRouteNode(route.name, route.path, route)
      ),
      ...data.apis.map((api) => createApiNode(api.name, api.name, api)),
      ...data.jobs.map((job) => createJobNode(job.name, job.name, job)),
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
      ...data.entities.map((entity) => createEdge(entity.name, data.app.name)),
      ...data.routes.map((route) => createEdge(route.name, route.toPage.name)),
      ...data.operations.flatMap((operation) =>
        operation.entities.map((entity) =>
          createEdge(operation.name, entity.name)
        )
      ),
      ...data.apis.flatMap((api) =>
        api.entities.map((entity) => createEdge(api.name, entity.name))
      ),
      ...data.jobs.flatMap((job) =>
        job.entities.map((entity) => createEdge(job.name, entity.name))
      ),
      ...data.routes.map((route) => createEdge(data.app.name, route.name)),
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
  }, [data, setNodes, setEdges, fitView]);

  // Calculate the initial layout on mount.
  useLayoutEffect(() => {
    onLayout();
  }, [onLayout]);

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
          setSelectedNode(node);
          onOpen();
        }}
      >
        <Background
          style={{
            backgroundColor: `hsl(var(--nextui-background)`,
          }}
          color={`#444`}
        />
        {/* <Controls /> */}
      </ReactFlow>

      <Modal size="lg" isOpen={isOpen} onClose={onClose}>
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
                {/* <Button color="danger" variant="light" onPress={onClose}>
                  Close
                </Button>
                <Button color="primary" onPress={onClose}>
                  Action
                </Button> */}
              </ModalFooter>
            </>
          )}
        </ModalContent>
      </Modal>
    </div>
  );
}

function getNodeHeight(node: Node) {
  if (node.type === "apiNode") {
    return 100;
  }
  if (node.type === "jobNode") {
    return 100;
  }
  return 50;
}
