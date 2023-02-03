import { CommonOptions } from '../CommonOptions.js';
import { ContextOption } from '../helpers/CommonOps.js';

export interface CmdOptions extends CommonOptions {
	context: ContextOption;
}
