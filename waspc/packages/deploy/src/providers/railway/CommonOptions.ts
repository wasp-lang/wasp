import { WaspCliExe, WaspProjectDir } from "../../common/brandedTypes.js";
import { RailwayCliExe } from "./brandedTypes.js";

export interface CommonOptions {
  waspExe: WaspCliExe;
  railwayExe: RailwayCliExe;
  waspProjectDir: WaspProjectDir;
}

export interface SecretsOptions {
  serverSecret: string[];
  // There are client runtime secrets, they are available to the server serving the static files.
  // They are useless right now (might be useful when we go with SSR) since our client can't access them.
  clientSecret: string[];
}
