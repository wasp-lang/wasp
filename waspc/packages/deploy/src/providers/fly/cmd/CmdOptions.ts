import { CommonOptions } from '../../shared/CommonOptions';
import { ContextOption } from '../helpers/CommonOps.js';

export interface CmdOptions extends CommonOptions {
	context: ContextOption;
}
