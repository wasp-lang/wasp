export const sleep = (ms: number): Promise<unknown> =>
  new Promise((r) => setTimeout(r, ms));
