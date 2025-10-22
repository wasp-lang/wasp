import { Decl, GetDeclForType } from "./appSpec"

export type NodeDataForDecl<T extends Decl["declType"]> = { name: GetDeclForType<T>["declName"], value: GetDeclForType<T>["declValue"], type: GetDeclForType<T>["declType"] }


