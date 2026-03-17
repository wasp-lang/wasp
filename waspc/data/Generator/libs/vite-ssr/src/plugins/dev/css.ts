import type { NormalizedHotChannel } from "vite";
import { PACKAGE_NAME } from "../common/constants";

export const REGISTER_CSS_EVENT_NAME = `${PACKAGE_NAME}:register-css` as const;

export interface RegisterCssData {
  id: string;
}

declare module "vite/types/customEvent.d.ts" {
  interface CustomEventMap {
    [REGISTER_CSS_EVENT_NAME]: RegisterCssData;
  }
}

const makeRegisterCssCode = (id: string) =>
  // We use the HMR event bus (as instructed by Vite
  // https://vite.dev/guide/api-environment-plugins#application-plugin-communication)
  // to register the CSS file path with the plugin once it is imported. We also
  // make sure to re-register it whenever the module is updated (on `prune`), so
  // that we can keep track of it.
  `
    if (import.meta.hot) {
      const register = () => import.meta.hot.send(${JSON.stringify(REGISTER_CSS_EVENT_NAME)}, { id: ${JSON.stringify(id)} });
      register();
      import.meta.hot.accept();
      import.meta.hot.prune(register);
    }
  `;

export const addRegisterCss = (id: string, code: string) =>
  [code, makeRegisterCssCode(id)].join(";\n");

export const withCollectingRegisteredCss = async <T>(
  hot: NormalizedHotChannel,
  fnRegistersCss: () => Promise<T>,
) => {
  const cssFiles = new Set<string>();

  const listener = ({ id }: RegisterCssData) => cssFiles.add(id);
  hot.on(REGISTER_CSS_EVENT_NAME, listener);

  try {
    const result = await fnRegistersCss();
    return { result, cssFiles };
  } finally {
    hot.off(REGISTER_CSS_EVENT_NAME, listener);
  }
};
