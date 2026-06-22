export type Clock = {
  now(): Date;
};

export type RandomSource = {
  integer(args: { min: number; max: number }): number;
};

export type WorkSimulator = {
  doFakeWork(): Promise<void>;
};
