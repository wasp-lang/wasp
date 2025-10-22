import { Edge, Position } from "reactflow";
import { AnyRef, Decl, GetDeclForType } from "../appSpec";
import { DeclNode } from "../node";

export function generateNodeId(declOrRef: Decl | AnyRef): string {
  // It's a Ref.
  if ("name" in declOrRef) {
    return `${declOrRef.declType.toLowerCase()}-${declOrRef.name}`;
  }
  return `${declOrRef.declType.toLowerCase()}-${declOrRef.declName}`;
}

export function createPageNode(
  page: GetDeclForType<"Page">,
  selectedNode: DeclNode | null,
): DeclNode {
  const id = generateNodeId(page);
  return {
    id,
    type: page.declType,
    data: { name: page.declName, value: page.declValue, type: page.declType },
    position: { x: 0, y: 0 },
    targetPosition: Position.Left,
    selected: selectedNode?.id === id,
  } satisfies DeclNode;
}

export function createActionNode(
  action: GetDeclForType<"Action">,
  selectedNode: DeclNode | null,
): DeclNode {
  const id = generateNodeId(action);
  return {
    id,
    type: action.declType,
    data: { name: action.declName, value: action.declValue, type: action.declType },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies DeclNode;
}

export function createQueryNode(
  query: GetDeclForType<"Query">,
  selectedNode: DeclNode | null,
): DeclNode {
  const id = generateNodeId(query);
  return {
    id,
    type: query.declType,
    data: { name: query.declName, value: query.declValue, type: query.declType },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies DeclNode;
}


export function createRouteNode(
  route: GetDeclForType<"Route">,
  selectedNode: DeclNode | null,
): DeclNode {
  const id = generateNodeId(route);
  return {
    id,
    type: route.declType,
    data: { name: route.declName, value: route.declValue, type: route.declType },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    targetPosition: Position.Left,
    selected: selectedNode?.id === id,
  } satisfies DeclNode;
}

export function createEntityNode(
  entity: GetDeclForType<"Entity">,
  selectedNode: DeclNode | null,
): DeclNode {
  const id = generateNodeId(entity);
  return {
    id,
    type: entity.declType,
    data: { name: entity.declName, value: entity.declValue, type: entity.declType },
    position: { x: 0, y: 0 },
    targetPosition: Position.Left,
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies DeclNode;
}

export function createApiNode(
  api: GetDeclForType<"Api">,
  selectedNode: DeclNode | null,
): DeclNode {
  const id = generateNodeId(api);
  return {
    id,
    type: api.declType,
    data: { name: api.declName, value: api.declValue, type: api.declType },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies DeclNode;
}

export function createJobNode(
  job: GetDeclForType<"Job">,
  selectedNode: DeclNode | null,
): DeclNode {
  const id = generateNodeId(job);
  return {
    id,
    type: job.declType,
    data: { name: job.declName, value: job.declValue, type: job.declType },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies DeclNode;
}

export function createAppNode(
  app: GetDeclForType<"App">,
  selectedNode: DeclNode | null,
): DeclNode {
  const id = generateNodeId(app);
  return {
    id,
    type: app.declType,
    data: { name: app.declName, value: app.declValue, type: app.declType },
    position: { x: 0, y: 0 },
    targetPosition: Position.Left,
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies DeclNode;
}

export function createEdge(
  sourceDeclOrRel: Decl | AnyRef,
  targetDeclOrRef: Decl | AnyRef,
  selectedNode: DeclNode | null,
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