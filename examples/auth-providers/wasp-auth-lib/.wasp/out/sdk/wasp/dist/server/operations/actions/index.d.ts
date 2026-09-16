import { type AuthenticatedOperationFor } from '../wrappers';
import type { FromRegisterPath } from '../../../types/register';
import type { CreateTask } from './types';
export type RegisteredCreateTask = FromRegisterPath<['operations', 'createTask'], CreateTask>;
export declare const createTask: AuthenticatedOperationFor<RegisteredCreateTask>;
//# sourceMappingURL=index.d.ts.map