import { ENVIRONMENT_NAMES, PACKAGE_NAME } from "./common/constants";

export const ssrSetEnvironments = () => ({
  name: `${PACKAGE_NAME}:set-environments`,

  config: () => ({
    environments: {
      // Just declaring the environments so they're available. These are
      // built-in to Vite so we don't need to set any specific values for them,
      // we're just being explicit.
      [ENVIRONMENT_NAMES.SSR]: {},
      [ENVIRONMENT_NAMES.CLIENT]: {},
    },
  }),
});
