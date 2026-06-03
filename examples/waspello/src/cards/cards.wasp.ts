import { action, query, type Decl } from "@wasp.sh/spec";

import { createCard, updateCard } from "./cards" with { type: "ref" };
import {
  createList,
  createListCopy,
  deleteList,
  getListsAndCards,
  updateList,
} from "./lists" with { type: "ref" };

export const cardsDecls: Decl[] = [
  query(getListsAndCards, { entities: ["List", "Card"] }),
  action(createList, { entities: ["List"] }),
  action(updateList, { entities: ["List"] }),
  action(deleteList, { entities: ["List", "Card"] }),
  action(createListCopy, { entities: ["List", "Card"] }),
  action(createCard, { entities: ["Card"] }),
  action(updateCard, { entities: ["Card"] }),
];
