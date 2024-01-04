import React, { useEffect, useRef, useState } from 'react'
import { Link, routes } from '@wasp/router'
import { User } from '@wasp/auth/types'
import api from '@wasp/api'
import {
  useSocket,
  useSocketListener,
  ServerToClientPayload,
} from '@wasp/webSocket'
import { getName, getProviderData } from '../user'

async function fetchCustomRoute() {
  const res = await api.get('/foo/bar')
  console.log(res.data)
}

export const ProfilePage = ({ user }: { user: User }) => {
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

  const providerData = getProviderData(user)

  return (
    <>
      <h2>Profile page</h2>
      <div>
        Hello <strong>{getName(user)}</strong>! Your status is{' '}
        <strong>
          {providerData && providerData.isEmailVerified
            ? 'verfied'
            : 'unverified'}
        </strong>
        .
      </div>
      <br />
      <Link to="/task/:id" params={{ id: 3 }}>
        Task 3
      </Link>
      <p>
        Route is{' '}
        {routes.TaskRoute.build({
          params: { id: 5 },
          search: { google: 'true' },
          hash: 'Miho',
        })}
      </p>
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
