/// <reference types="node" />

export type Command = {
  name: string;
  argsHint?: string;
  description: string;
  run: (args: string[]) => Promise<void>;
};
