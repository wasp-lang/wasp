import { WebSocketClientConfigFn } from 'wasp/client/webSocket'

export const getConfig: WebSocketClientConfigFn = () => ({
  transports: ['polling'],
})
