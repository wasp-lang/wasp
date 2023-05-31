{{={= =}=}}

import { useState, useEffect } from 'react'
import { io, Socket } from 'socket.io-client'
import { DefaultEventsMap, EventsMap } from '@socket.io/component-emitter'

import { getAuthToken, events as apiEvents } from './api'
import config from './config'

// TODO: In the future, it would be nice if users could pass more
// options to `io`, likely via some `configFn`.
export const socket = io(config.apiUrl, { autoConnect: {= autoConnect =} })

function refreshAuthToken() {
  // NOTE: In the future, we should consider making this explicit in the Wasp file when
  // we make the change for how auth works in Operations.
  // For now, it is fine, and can be ignored on the server if not needed.
  socket.auth = {
    token: getAuthToken()
  }

  if (socket.connected) {
    socket.disconnect()
    socket.connect()
  }
}

refreshAuthToken()
apiEvents.on('authToken.set', refreshAuthToken)
apiEvents.on('authToken.clear', refreshAuthToken)

export function useSocket<
  ServerToClientEvents extends EventsMap = DefaultEventsMap,
  ClientToServerEvents extends EventsMap = DefaultEventsMap
>(): {
  socket: Socket<ServerToClientEvents, ClientToServerEvents>;
  isConnected: boolean;
  registerHandler: typeof registerHandler;
} {
  const [isConnected, setIsConnected] = useState(socket.connected)

  useEffect(() => {
    function onConnect() {
      setIsConnected(true)
    }

    function onDisconnect() {
      setIsConnected(false)
    }

    socket.on('connect', onConnect)
    socket.on('disconnect', onDisconnect)

    return () => {
      socket.off('connect', onConnect)
      socket.off('disconnect', onDisconnect)
    }
  }, [])

  function registerHandler<Event extends keyof ServerToClientEvents>
    (event: Event, cb: ServerToClientEvents[Event]) {
    useEffect(() => {
      socket.on(event, cb)
      return () => {
        socket.off(event, cb)
      }
    }, [])
  }  

  return { socket, isConnected, registerHandler }
}
