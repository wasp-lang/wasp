export type PrerenderFn = (
  route: string,
  ctx: PrerenderContext,
) => Promise<Response | null>;

export interface PrerenderContext {
  transformIndexHtml: (html: string) => Promise<string>;
  clientEntrySrc: string;
}

export type PrerenderPaths = readonly string[];
