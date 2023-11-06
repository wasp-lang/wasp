import { createContext } from "react";

const DND_ITEM_POS_SPACING = 2 ** 16;

// It is expected that each item has .pos property.
export const calcNewDndItemPos = (items) => {
  if (!Array.isArray(items) || items.length === 0)
    return DND_ITEM_POS_SPACING - 1;

  return Math.max(...items.map((l) => l.pos)) + DND_ITEM_POS_SPACING;
};

// It is assummed that items are sorted by pos, ascending.
export const calcNewPosOfDndItemMovedWithinList = (items, srcIdx, destIdx) => {
  if (srcIdx === destIdx) return items[srcIdx].pos;
  if (destIdx === 0) return items[0].pos / 2;
  if (destIdx === items.length - 1)
    return items[items.length - 1].pos + DND_ITEM_POS_SPACING;

  if (destIdx > srcIdx)
    return (items[destIdx].pos + items[destIdx + 1].pos) / 2;
  if (destIdx < srcIdx)
    return (items[destIdx - 1].pos + items[destIdx].pos) / 2;
};

// It is assummed that items is sorted by pos, ascending.
export const calcNewPosOfDndItemInsertedInAnotherList = (items, destIdx) => {
  if (items.length === 0) return DND_ITEM_POS_SPACING - 1;
  if (destIdx === 0) return items[0].pos / 2;
  if (destIdx === items.length)
    return items[items.length - 1].pos + DND_ITEM_POS_SPACING;

  return (items[destIdx - 1].pos + items[destIdx].pos) / 2;
};

/**
 * @typedef {Object} PositionContextValue
 * @property {function(number): number} getPosOfNewItemInsertedinAnotherListBefore Get a new position after the item in the given index.
 * @property {function(number): number} getPosOfItemInsertedInAnotherListAfter Get a new position before the item in the given index.
 * @property {function(number, number): number} getPosOfItemMovedWithinList Get a new position for an item moving from one index to another.
 * @property {function(): number} getPosOfNewItem Get a new position for an item being inserted at the end of the list.
 */

/**
 * @type {import('react').Context<PositionContextValue>}
 */
export const PositionContext = createContext({
  getPosOfNewItemInsertedinAnotherListBefore: undefined,
  getPosOfItemInsertedInAnotherListAfter: undefined,
  getPosOfItemMovedWithinList: undefined,
  getPosOfNewItem: undefined,
});

export const PositionProvider = ({ items, children }) => {
  /**
   * @type {PositionContextValue}
   */
  const value = {
    getPosOfItemInsertedInAnotherListAfter: (idx) =>
      calcNewPosOfDndItemInsertedInAnotherList(items, idx + 1),
    getPosOfNewItemInsertedinAnotherListBefore: (idx) =>
      calcNewPosOfDndItemInsertedInAnotherList(items, idx),
    getPosOfNewItem: () => calcNewDndItemPos(items),
    getPosOfItemMovedWithinList: (srcIdx, destIdx) =>
      calcNewPosOfDndItemMovedWithinList(items, srcIdx, destIdx),
  };

  return (
    <PositionContext.Provider value={value}>
      {children}
    </PositionContext.Provider>
  );
};
