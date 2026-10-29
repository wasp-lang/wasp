import type { NormalizedHotChannel } from "vite";
import { PACKAGE_NAME } from "../common/constants";
import { appendToHead } from "../util/html";

export const REGISTER_CSS_EVENT_NAME = `${PACKAGE_NAME}:register-css` as const;

export interface RegisterCssData {
  id: string;
}

declare module "vite/types/customEvent.d.ts" {
  interface CustomEventMap {
    [REGISTER_CSS_EVENT_NAME]: RegisterCssData;
  }
}

export const appendCssRegistrationCode = (id: string, code: string) =>
  code +
  // Ensure any previous statement is properly terminated.
  ";\n" +
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

export const withRegisteredCss = async (
  hot: NormalizedHotChannel,
  makeHtml: () => Promise<string | null>,
) => {
  const cssFiles = new Set<string>();

  const listener = ({ id }: RegisterCssData) => cssFiles.add(id);
  hot.on(REGISTER_CSS_EVENT_NAME, listener);

  try {
    const html = await makeHtml();
    if (!html) {
      return html;
    }

    const htmlWithCss = appendToHead(
      html,
      Array.from(cssFiles)
        .map((id) => `<link rel="stylesheet" href="${id}">\n`)
        .join(""),
    );

    return htmlWithCss;
  } finally {
    hot.off(REGISTER_CSS_EVENT_NAME, listener);
  }
};
