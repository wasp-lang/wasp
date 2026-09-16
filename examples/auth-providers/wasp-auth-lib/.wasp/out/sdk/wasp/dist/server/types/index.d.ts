import { type Application } from 'express';
import { Server } from 'http';
export type ServerSetupFn = (context: ServerSetupFnContext) => Promise<void>;
export type ServerSetupFnContext = {
    app: Application;
    server: Server;
};
//# sourceMappingURL=index.d.ts.map