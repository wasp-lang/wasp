import { action, query } from "wasp-config";

import { createCard, updateCard } from "@src/cards/cards";
import {
  createList,
  createListCopy,
  deleteList,
  getListsAndCards,
  updateList,
} from "@src/cards/lists";

export const cards = [
  query(getListsAndCards, { entities: ["List", "Card"] }),
  action(createList, { entities: ["List"] }),
  action(updateList, { entities: ["List"] }),
  action(deleteList, { entities: ["List", "Card"] }),
  action(createListCopy, { entities: ["List", "Card"] }),
  action(createCard, { entities: ["Card"] }),
  action(updateCard, { entities: ["Card"] }),
];
