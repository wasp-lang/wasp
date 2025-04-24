import { Emitter } from 'mitt';
type ApiEvents = {
    'sessionId.set': void;
    'sessionId.clear': void;
};
export declare const apiEventsEmitter: Emitter<ApiEvents>;
export {};
//# sourceMappingURL=events.d.ts.map