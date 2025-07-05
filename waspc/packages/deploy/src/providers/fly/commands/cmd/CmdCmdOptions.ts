import { CommonCmdOptions } from "../../CommonCmdOptions";
import { ContextOption } from "../../CommonOps";

export interface CmdCmdOptions extends CommonCmdOptions {
  context: ContextOption;
}
