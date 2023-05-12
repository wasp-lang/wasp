import { useState, useEffect } from 'react'
import { socket } from '@wasp/socket'
import { getAuthToken } from '@wasp/api'

export default function useSocket({ autoConnect = true, includeAuth = true } = {}): [boolean, typeof socket] {
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
