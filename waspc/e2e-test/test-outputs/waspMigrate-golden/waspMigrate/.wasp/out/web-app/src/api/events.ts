import mitt, { Emitter } from 'mitt';

type ApiEvents = {
  // key: Event name
  // type: Event payload type
  'sessionId.set': void;
  'sessionId.clear': void;
};

// Used to allow API clients to register for auth session ID change events.
export const apiEventsEmitter: Emitter<ApiEvents> = mitt<ApiEvents>();
