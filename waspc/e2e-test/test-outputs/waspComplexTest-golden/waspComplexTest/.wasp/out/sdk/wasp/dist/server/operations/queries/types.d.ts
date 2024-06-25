import { type _User, type AuthenticatedQueryDefinition, type Payload } from 'wasp/server/_types';
export type MySpecialQuery<Input extends Payload = never, Output extends Payload = Payload> = AuthenticatedQueryDefinition<[
    _User
], Input, Output>;
