import type { Task } from "wasp/entities";
import type { CreateTask, GetMyTasks } from "wasp/server/operations";
/**
 * These two operations are byte-for-byte identical in all three example apps.
 *
 * That is the entire point of the exercise. `context.user` is a row in this app's
 * own `User` table with this app's own id type, whether the request was verified
 * by Wasp's auth, by Better Auth, or by Clerk.
 */
export declare const getMyTasks: GetMyTasks<void, Task[]>;
export declare const createTask: CreateTask<{
    description: string;
}, Task>;
