import PgBoss from 'pg-boss';
export declare const pgBossStarted: Promise<PgBoss>;
/**
 * Prepares the target PostgreSQL database and begins job monitoring.
 * If the required database objects do not exist in the specified database,
 * `boss.start()` will automatically create them.
 * Ref: https://github.com/timgit/pg-boss/blob/master/docs/readme.md#start
 *
 * After making this call, we can send pg-boss jobs and they will be persisted and acted upon.
 * This should only be called once during a server's lifetime.
 */
export declare function startPgBoss(): Promise<void>;
//# sourceMappingURL=pgBoss.d.ts.map