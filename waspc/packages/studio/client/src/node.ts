import { Node } from "reactflow";
import { Decl, DeclTypeToValue, GetDeclForType } from "./appSpec";
import { ApiNode } from "./graph/ApiNode";
import { AppNode } from "./graph/AppNode";
import { EntityNode } from "./graph/EntityNode";
import { JobNode } from "./graph/JobNode";
import { ActionNode, QueryNode } from "./graph/OperationNode";
import { PageNode } from "./graph/PageNode";
import { RouteNode } from "./graph/RouteNode";

export const declNodeTypes = {
  Page: PageNode,
  Entity: EntityNode,
  Query: QueryNode,
  Action: ActionNode,
  App: AppNode,
  Route: RouteNode,
  Api: ApiNode,
  Job: JobNode,
} as const;

export type NodeDataForDecl<T extends Decl["declType"]> = { name: GetDeclForType<T>["declName"], value: GetDeclForType<T>["declValue"], type: GetDeclForType<T>["declType"] }

export type DeclNode = {
  [Type in keyof DeclTypeToValue]: Node<NodeDataForDecl<Type>, Type>
}[keyof DeclTypeToValue];
