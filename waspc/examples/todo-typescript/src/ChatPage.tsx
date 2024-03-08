import React, { useEffect, useRef, useState } from 'react'
import { api } from 'wasp/client/api'
import {
  useSocket,
  useSocketListener,
  ServerToClientPayload,
} from 'wasp/client/webSocket'

async function fetchCustomRoute() {
  const res = await api.get('/foo/bar')
  console.log(res.data)
}

export const ChatPage = () => {
  const [messages, setMessages] = useState<
    ServerToClientPayload<'chatMessage'>[]
  >([])
  const { socket, isConnected } = useSocket()
  const inputRef = useRef<HTMLInputElement>(null)

  useEffect(() => {
    fetchCustomRoute()
  }, [])

  useSocketListener('chatMessage', (msg) =>
    setMessages((priorMessages) => [msg, ...priorMessages])
  )

  function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault()

    if (inputRef.current !== null) {
      socket.emit('chatMessage', inputRef.current.value)
      inputRef.current.value = ''
    }
  }

  const messageList = messages.map((msg) => (
    <li key={msg.id}>
      <em>{msg.username}</em>: {msg.text}
    </li>
  ))
  const connectionIcon = isConnected ? '🟢' : '🔴'

  return (
    <>
      <h2 className="text-2xl font-bold mb-4">Chats</h2>
      <div>
        <form onSubmit={handleSubmit}>
          <div className="flex space-x-4 place-items-center">
            <div>{connectionIcon}</div>
            <div>
              <input type="text" ref={inputRef} />
            </div>
            <div>
              <button className="btn btn-primary" type="submit">
                Submit
              </button>
            </div>
          </div>
        </form>
        <ul>{messageList}</ul>
      </div>
    </>
  )
}
