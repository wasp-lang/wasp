import { useState, useEffect } from 'react'
import { socket } from '@wasp/socket'
import { getAuthToken } from '@wasp/api'

export default function useSocket({ autoConnect = true, includeAuth = true } = {}): [boolean, typeof socket] {
  const [isConnected, setIsConnected] = useState(socket.connected)

  if (includeAuth) {
    socket.auth = {
      token: getAuthToken()
    }
  }

  if (autoConnect) {
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

    return () => {
      socket.off('connect', onConnect)
      socket.off('disconnect', onDisconnect)

      if (autoConnect) {
        socket.disconnect()
      }
    }
  }, [])

  return [isConnected, socket]
}
