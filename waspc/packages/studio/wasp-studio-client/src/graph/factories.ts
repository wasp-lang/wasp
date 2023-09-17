import { Position, Node, Edge } from "reactflow";

let id = 1;

export function generateId() {
  return `${id++}`;
}

type AnyData = {
  [key: string]: unknown;
};

export function createPageNode(id: string, name: string, data: AnyData) {
  return {
    id,
    type: "pageNode",
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    // sourcePosition: Position.Right,
    targetPosition: Position.Left,
    // type: "input",
  } satisfies Node;
}

export function createActionNode(id: string, name: string, data: AnyData) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    // targetPosition: Position.Left,
    type: "actionNode",
  } satisfies Node;
}

export function createQueryNode(id: string, name: string, data: AnyData) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    // targetPosition: Position.Left,
    type: "queryNode",
  } satisfies Node;
}

export function createRouteNode(id: string, name: string, data: AnyData) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    targetPosition: Position.Left,
    type: "routeNode",
  } satisfies Node;
}

export function createEntityNode(
  id: string,
  name: string,
  isUserEntity: boolean,
  data: AnyData
) {
  return {
    id,
    data: { label: name, isUserEntity, ...data },
    position: { x: 0, y: 0 },
    type: "entityNode",
    targetPosition: Position.Left,
    sourcePosition: Position.Right,
    // style: {
    //   backgroundColor: `#fde047`,
    // },
  } satisfies Node;
}

export function createApiNode(id: string, name: string, data: AnyData) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    type: "apiNode",
    sourcePosition: Position.Right,
  } satisfies Node;
}

export function createJobNode(id: string, name: string, data: AnyData) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    type: "jobNode",
    sourcePosition: Position.Right,
  } satisfies Node;
}

export function createAppNode(id: string, name: string, data: AnyData) {
  return {
    id,
    data: { label: name, ...data },
    position: { x: 0, y: 0 },
    type: "appNode",
    targetPosition: Position.Left,
    sourcePosition: Position.Right,
  } satisfies Node;
}

export function createEdge(source: string, target: string, label?: string) {
  return {
    id: `${source}-${target}`,
    source,
    target,
    animated: true,
    label,
  } satisfies Edge;
}
