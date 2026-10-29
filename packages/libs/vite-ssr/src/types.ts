export type PrerenderFn = (
  route: string,
  ctx: PrerenderContext,
) => Promise<string | null>;

export interface PrerenderContext {
  clientEntrySrc: string;
}

export type PrerenderPaths = readonly string[];
