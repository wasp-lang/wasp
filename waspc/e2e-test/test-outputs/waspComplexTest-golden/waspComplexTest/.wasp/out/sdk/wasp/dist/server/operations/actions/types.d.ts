import { type _User, type AuthenticatedActionDefinition, type Payload } from 'wasp/server/_types';
export type MySpecialAction<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedActionDefinition<[
    _User
], Input, Output>;
