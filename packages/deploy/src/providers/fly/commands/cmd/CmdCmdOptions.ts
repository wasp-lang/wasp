import { CommonCmdOptions } from "../../CommonCmdOptions.js";
import { ContextOption } from "../../CommonOps.js";

export interface CmdCmdOptions extends CommonCmdOptions {
  context: ContextOption;
}
