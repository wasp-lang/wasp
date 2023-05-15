import { WaspSocketData } from '@wasp/universal/types'

export interface ServerToClientEvents {
  chatMessage: (msg: { id: string, username: string, text: string }) => void;
}

export interface ClientToServerEvents {
  chatMessage: (msg: string) => void;
}

export interface InterServerEvents {}

export interface SocketData extends WaspSocketData {}
