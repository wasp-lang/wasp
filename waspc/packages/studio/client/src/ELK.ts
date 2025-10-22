import ELK, { type ElkNode } from "elkjs/lib/elk.bundled.js";
import {
  Edge,
  Node
} from "reactflow";

const elk = new ELK();

export const getLayoutedElements = (nodes: Node[], edges: Edge[]) => {
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
    children: nodes.map((node: Node) => {
      console.log(node);
      return {
        ...node,
        width: getNodeWidth(node),
        height: getNodeHeight(node),
      }
    }),
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

function getNodeHeight(node: Node ) {
  if (node.type === "apiNode") {
    return 100;
  }
  if (node.type === "jobNode" && node.data.schedule) {
    return 100;
  }
  if (node.type === "appNode") {
    const authMethods = (Object.values(node.data.value.auth?.methods ?? {}));
    return 100 + authMethods.length * 50;
  }
  return 50;
}

function getNodeWidth(node: Node) {
  const textCandidates = [
    node.data.name,
  ]
    .filter(Boolean)
    .map((text) => text.length);

  const longestText = Math.max(...textCandidates);
  const width = Math.max(150, longestText * 10 + 40);
  return width;
}

// function getAuthMethods(node: Node) {
//   if (node.type !== "appNode") {
//     return [];
//   }
//   return (
//     node.data?.auth?.methods.map((method: string) => `Auth: ${method}`) ?? []
//   );
// }