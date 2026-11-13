import {
  Background,
  Edge,
  ReactFlow,
  useEdgesState,
  useNodesState,
  useReactFlow,
} from "@xyflow/react";
import { useEffect, useMemo, useState } from "react";
import { flushSync } from "react-dom";
import { Data } from "./types";

import ELK, { type ElkNode } from "elkjs/lib/elk.bundled.js";

import { ApiNode } from "./graph/Api";
import { AppNode } from "./graph/App";
import { EntityNode } from "./graph/Entity";
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
  type StudioNode,
} from "./graph/factories";
import { JobNode } from "./graph/Job";
import { ActionNode, QueryNode } from "./graph/Operation";
import { PageNode } from "./graph/Page";
import { RouteNode } from "./graph/Route";
import { Loading } from "./Loading";

const elk = new ELK();

const getLayoutedElements = (nodes: StudioNode[], edges: Edge[]) => {
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
    children: nodes.map((node: StudioNode) => ({
      ...node,
      width: node.measured?.width,
      height: node.measured?.height,
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
          // React Flow treats `width` and `height` as fixed node dimensions,
          // so we drop them and let nodes keep sizing to their content.
          // eslint-disable-next-line @typescript-eslint/no-unused-vars
          nodes: layoutedGraph.children.map(({ width, height, ...node }) => ({
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
  // NOTE: This is not used. But it might be useful in the future.
  const [selectedNode] = useState<StudioNode | null>(null);

  const [nodes, setNodes, onNodesChange] = useNodesState<StudioNode>([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState<Edge>([]);
  const { fitView } = useReactFlow();
  const [laidOutData, setLaidOutData] = useState<Data | null>(null);

  // On new data, render the new nodes where the old ones were, so React Flow
  // can measure them for the layout below while the graph stays on screen.
  // See https://react.dev/learn/you-might-not-need-an-effect#adjusting-some-state-when-a-prop-changes
  const [renderedData, setRenderedData] = useState<Data | null>(null);
  if (data !== renderedData) {
    setRenderedData(data);
    setNodes((oldNodes) => {
      const oldPositions = new Map(
        oldNodes.map((node) => [node.id, node.position]),
      );
      return createNodes(data, selectedNode).map((node) => ({
        ...node,
        position: oldPositions.get(node.id) ?? node.position,
      }));
    });
    setEdges(createEdges(data, selectedNode));
  }
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

  // Once React Flow has measured the nodes, lay them out with their real sizes.
  // We check `measured` instead of using `useNodesInitialized()`, because that
  // one can still report the previous data's nodes as measured when new data
  // arrives.
  useEffect(() => {
    const areNodesMeasured = nodes.every(
      ({ measured }) =>
        measured?.width !== undefined && measured?.height !== undefined,
    );
    if (laidOutData === data || !areNodesMeasured) {
      return;
    }

    let isCancelled = false;
    getLayoutedElements(nodes, edges).then((result) => {
      if (isCancelled || !result) {
        return;
      }
      const { nodes: layoutedNodes, edges: layoutedEdges } = result;
      if (!layoutedNodes || !layoutedEdges) {
        return;
      }
      // React Flow syncs `nodes` into its store and applies `fitView()` in
      // effects, which can run after a paint. Flushing them makes the first
      // uncovered frame show the laid out and fitted graph.
      flushSync(() => {
        // Hack
        setNodes(layoutedNodes as StudioNode[]);
        // Hack
        setEdges(layoutedEdges as unknown as Edge[]);
        setLaidOutData(data);
        // Only fit on the first layout, so updates keep the user's viewport.
        if (laidOutData === null) {
          fitView();
        }
      });
    });

    return () => {
      isCancelled = true;
    };
  }, [data, laidOutData, nodes, edges, setNodes, setEdges, fitView]);

  return (
    <div className="relative h-full">
      {/* Covers the nodes while they're first measured and laid out. */}
      {laidOutData === null && (
        <div className="bg-wasp-bg absolute inset-0 z-10">
          <Loading />
        </div>
      )}
      <ReactFlow
        nodes={nodes}
        edges={edges}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        fitView
        nodeTypes={nodeTypes}
      >
        <Background
          style={{
            backgroundColor: `var(--color-wasp-bg)`,
          }}
          color={`var(--color-wasp-g3)`}
          gap={16}
        />
      </ReactFlow>
    </div>
  );
}

function generateId(name: string, type: string): string {
  return `${type}:${name}`;
}

function createNodes(
  data: Data,
  selectedNode: StudioNode | null,
): StudioNode[] {
  return [
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
}

function createEdges(data: Data, selectedNode: StudioNode | null): Edge[] {
  return [
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
}
