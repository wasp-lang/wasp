import { Edge, Node, Position } from "reactflow";
import { AnyRef, Decl, GetDeclForType } from "../appSpec";

export function generateNodeId(declOrRef: Decl | AnyRef): string {
  // It's a Ref.
  if ("name" in declOrRef) {
    return `${declOrRef.declType.toLowerCase()}-${declOrRef.name}`;
  }
  return `${declOrRef.declType.toLowerCase()}-${declOrRef.declName}`;
}

// app: GetDeclForType<"App">;
// pages: GetDeclForType<"Page">[];
// routes: GetDeclForType<"Route">[];
// entities: GetDeclForType<"Entity">[];
// actions: GetDeclForType<"Action">[];
// queries: GetDeclForType<"Query">[];
// apis: GetDeclForType<"Api">[];
// jobs: GetDeclForType<"Job">[];

export function createPageNode(
  page: GetDeclForType<"Page">,
  selectedNode: Node | null,
): Node {
  const id = generateNodeId(page);
  return {
    id,
    type: `${page.declType.toLowerCase()}Node`,
    data: { name: page.declName, value: page.declValue, type: page.declType },
    position: { x: 0, y: 0 },
    targetPosition: Position.Left,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createActionNode(
  action: GetDeclForType<"Action">,
  selectedNode: Node | null,
): Node {
  const id = generateNodeId(action);
  return {
    id,
    type: `${action.declType.toLowerCase()}Node`,
    data: { name: action.declName, value: action.declValue, type: action.declType },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createQueryNode(
  query: GetDeclForType<"Query">,
  selectedNode: Node | null,
): Node {
  const id = generateNodeId(query);
  return {
    id,
    type: `${query.declType.toLowerCase()}Node`,
    data: { name: query.declName, value: query.declValue, type: query.declType },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies Node;
}


export function createRouteNode(
  route: GetDeclForType<"Route">,
  selectedNode: Node | null,
): Node {
  const id = generateNodeId(route);
  return {
    id,
    type: `${route.declType.toLowerCase()}Node`,
    data: { name: route.declName, value: route.declValue, type: route.declType },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    targetPosition: Position.Left,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createEntityNode(
  entity: GetDeclForType<"Entity">,
  selectedNode: Node | null,
): Node {
  const id = generateNodeId(entity);
  return {
    id,
    type: `${entity.declType.toLowerCase()}Node`,
    data: { name: entity.declName, value: entity.declValue, type: entity.declType },
    position: { x: 0, y: 0 },
    targetPosition: Position.Left,
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createApiNode(
  api: GetDeclForType<"Api">,
  selectedNode: Node | null,
): Node {
  const id = generateNodeId(api);
  return {
    id,
    type: `${api.declType.toLowerCase()}Node`,
    data: { name: api.declName, value: api.declValue, type: api.declType },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createJobNode(
  job: GetDeclForType<"Job">,
  selectedNode: Node | null,
): Node {
  const id = generateNodeId(job);
  return {
    id,
    type: `${job.declType.toLowerCase()}Node`,
    data: { name: job.declName, value: job.declValue, type: job.declType },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createAppNode(
  app: GetDeclForType<"App">,
  selectedNode: Node | null,
): Node {
  const id = generateNodeId(app);
  return {
    id,
    type: `${app.declType.toLowerCase()}Node`,
    data: { name: app.declName, value: app.declValue, type: app.declType },
    position: { x: 0, y: 0 },
    targetPosition: Position.Left,
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createEdge(
  sourceDeclOrRel: Decl | AnyRef,
  targetDeclOrRef: Decl | AnyRef,
  selectedNode: Node | null,
) {
  const source = generateNodeId(sourceDeclOrRel);
  const target = generateNodeId(targetDeclOrRef);

  return {
    id: `${source}-${target}`,
    source: source,
    target: target,
    animated: true,
    selected: selectedNode?.id === source || selectedNode?.id === target,
  } satisfies Edge;
}