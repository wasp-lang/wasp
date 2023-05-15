import { useState, useEffect } from 'react'
import { Socket } from 'socket.io-client'
import { DefaultEventsMap, EventsMap } from '@socket.io/component-emitter'
import { socket } from './socket'
import { getAuthToken } from './api'

export function useSocket<
  ServerToClientEvents extends EventsMap = DefaultEventsMap,
  ClientToServerEvents extends EventsMap = ServerToClientEvents
>
  ({ autoConnect = true, includeAuth = true } = {}):
  [boolean, Socket<ServerToClientEvents, ClientToServerEvents>] {

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

    if (includeAuth) {
      socket.auth = {
        token: getAuthToken()
      }
    }

    if (autoConnect) {
      socket.connect()
    }

    return () => {
      if (autoConnect) {
        socket.disconnect()
      }

      socket.off('connect', onConnect)
      socket.off('disconnect', onDisconnect)
    }
  }, [])

  return [isConnected, socket]
}
