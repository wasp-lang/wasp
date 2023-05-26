import { useState, useEffect } from 'react'
import { io, Socket } from 'socket.io-client'
import { DefaultEventsMap, EventsMap } from '@socket.io/component-emitter'

import { getAuthToken } from './api'
import config from './config'

// TODO: In the future, it would be nice if users could configure this somehow.
const socket = io(config.apiUrl)

export function useSocket<
  ServerToClientEvents extends EventsMap = DefaultEventsMap,
  ClientToServerEvents extends EventsMap = DefaultEventsMap
>(): {
  socket: Socket<ServerToClientEvents, ClientToServerEvents>;
  isConnected: boolean;
  refreshAuthToken: () => void
} {

  const [isConnected, setIsConnected] = useState(socket.connected)

  // TODO: Any better way to do this?
  // It doesn't seem to detect updates while a connection is active, and this works. :/
  const refreshAuthToken = () => {
    socket.disconnect()
    // NOTE: In the future, we should consider making this explicit in the Wasp file when
    // we make the change for how auth works in Operations.
    // For now, it is fine, and can be ignored on the server if not needed.
    socket.auth = {
      token: getAuthToken()
    }
    socket.connect()
  }

  useEffect(() => {
    function onConnect() {
      setIsConnected(true)
    }

    function onDisconnect() {
      setIsConnected(false)
    }

    socket.on('connect', onConnect)
    socket.on('disconnect', onDisconnect)

    refreshAuthToken()

    return () => {
      socket.off('connect', onConnect)
      socket.off('disconnect', onDisconnect)
    }
  }, [])

  return { socket, isConnected, refreshAuthToken }
}
