declare module "wasp" {
  /** Hello! */
  export function placeholder(
    /** some placeholder comment */
    foo: string,
  ): string;
}

declare module "wasp/utils" {
  /** An utility function that I just created */
  export function placeholder2(/** bar */ date: Date): void;
}
