import { WaspSocketData } from '@wasp/universal/webSocket/types'

export interface ServerToClientEvents {
  chatMessage: (msg: { id: string, username: string, text: string }) => void;
}

export interface ClientToServerEvents {
  chatMessage: (msg: string) => void;
}

export interface InterServerEvents { }

export interface SocketData extends WaspSocketData { }
