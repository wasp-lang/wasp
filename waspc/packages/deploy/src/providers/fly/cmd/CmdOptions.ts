import { GlobalOptions } from '../GlobalOptions.js';
import { ContextOption } from '../helpers/CommonOps.js';

export interface CmdOptions extends GlobalOptions {
	context: ContextOption;
}
