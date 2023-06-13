import mitt, { Emitter } from 'mitt';

type ApiEvents = {
  // key: Event name
  // type: Event payload type
  'authToken.set': void;
  'authToken.clear': void;
};

// Used to allow API clients to register for auth token change events.
export const apiEventsEmitter: Emitter<ApiEvents> = mitt<ApiEvents>();
