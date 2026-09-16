import { type AuthenticatedOperationFor } from '../wrappers';
import type { FromRegisterPath } from '../../../types/register';
import type { GetMyTasks } from './types';
export type RegisteredGetMyTasks = FromRegisterPath<['operations', 'getMyTasks'], GetMyTasks>;
export declare const getMyTasks: AuthenticatedOperationFor<RegisteredGetMyTasks>;
//# sourceMappingURL=index.d.ts.map