import { ENVIRONMENT_NAMES, PACKAGE_NAME } from "./common/constants";

export const ssrSetEnvironments = () => ({
  name: `${PACKAGE_NAME}:set-environments`,

  config: () => ({
    environments: {
      [ENVIRONMENT_NAMES.SSR]: {},
      [ENVIRONMENT_NAMES.CLIENT]: {},
    },
  }),
});
