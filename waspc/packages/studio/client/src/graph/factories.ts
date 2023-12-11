import { Position, Node, Edge } from "reactflow";

let id = 1;

export function generateId() {
  return `${id++}`;
}

type AnyData = {
  [key: string]: unknown;
};

export function createPageNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: Node | null
) {
  return {
    id,
    type: "pageNode",
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    targetPosition: Position.Left,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createActionNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: Node | null
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    type: "actionNode",
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createQueryNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: Node | null
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    type: "queryNode",
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createRouteNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: Node | null
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    targetPosition: Position.Left,
    type: "routeNode",
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createEntityNode(
  id: string,
  name: string,
  isUserEntity: boolean,
  data: AnyData,
  selectedNode: Node | null
) {
  return {
    id,
    data: { label: name, isUserEntity, ...data },
    position: { x: 0, y: 0 },
    type: "entityNode",
    targetPosition: Position.Left,
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createApiNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: Node | null
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    type: "apiNode",
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createJobNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: Node | null
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    type: "jobNode",
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createAppNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: Node | null
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    type: "appNode",
    targetPosition: Position.Left,
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies Node;
}

export function createEdge(
  source: string,
  target: string,
  selectedNode: Node | null
) {
  return {
    id: `${source}-${target}`,
    source,
    target,
    animated: true,
    selected: selectedNode?.id === source || selectedNode?.id === target,
  } satisfies Edge;
}
