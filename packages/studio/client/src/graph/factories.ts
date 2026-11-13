import { Edge, Node, Position } from "@xyflow/react";

// Node data comes untyped from the Wasp Studio data file.
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type StudioNode = Node<Record<string, any>>;

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
  selectedNode: StudioNode | null,
) {
  return {
    id,
    type: "pageNode",
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    targetPosition: Position.Left,
    selected: selectedNode?.id === id,
  } satisfies StudioNode;
}

export function createActionNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: StudioNode | null,
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    type: "actionNode",
    selected: selectedNode?.id === id,
  } satisfies StudioNode;
}

export function createQueryNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: StudioNode | null,
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    type: "queryNode",
    selected: selectedNode?.id === id,
  } satisfies StudioNode;
}

export function createRouteNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: StudioNode | null,
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    targetPosition: Position.Left,
    type: "routeNode",
    selected: selectedNode?.id === id,
  } satisfies StudioNode;
}

export function createEntityNode(
  id: string,
  name: string,
  isUserEntity: boolean,
  data: AnyData,
  selectedNode: StudioNode | null,
) {
  return {
    id,
    data: { label: name, isUserEntity, ...data },
    position: { x: 0, y: 0 },
    type: "entityNode",
    targetPosition: Position.Left,
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies StudioNode;
}

export function createApiNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: StudioNode | null,
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    type: "apiNode",
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies StudioNode;
}

export function createJobNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: StudioNode | null,
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    type: "jobNode",
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies StudioNode;
}

export function createAppNode(
  id: string,
  name: string,
  data: AnyData,
  selectedNode: StudioNode | null,
) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    type: "appNode",
    targetPosition: Position.Left,
    sourcePosition: Position.Right,
    selected: selectedNode?.id === id,
  } satisfies StudioNode;
}

export function createEdge(
  source: string,
  target: string,
  selectedNode: StudioNode | null,
) {
  return {
    id: `${source}-${target}`,
    source,
    target,
    animated: true,
    selected: selectedNode?.id === source || selectedNode?.id === target,
  } satisfies Edge;
}
