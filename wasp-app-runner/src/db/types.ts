export type RunAppWithDbFn = (
  options: { appName: string; pathToApp: string },
  runApp: RunAppFn
) => Promise<void>;

export type RunAppFn = (context: {
  extraEnv: Record<string, string>;
}) => Promise<void>;
