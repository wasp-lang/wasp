import { useState, useEffect } from 'react'
import { Socket } from 'socket.io-client'
import { DefaultEventsMap, EventsMap } from '@socket.io/component-emitter'
import { socket } from './socket'
import { getAuthToken } from './api'

export function useSocket<
  ServerToClientEvents extends EventsMap = DefaultEventsMap,
  ClientToServerEvents extends EventsMap = ServerToClientEvents
>(): {
  socket: Socket<ServerToClientEvents, ClientToServerEvents>;
  isConnected: boolean;
  refreshAuthToken: () => void
} {

  const [isConnected, setIsConnected] = useState(socket.connected)

  const refreshAuthToken = () => {
    // TODO: Any better way to do this?
    // It doesn't seem to detect updates while a connection is active, and this works. :/
    socket.disconnect()
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
