import React, { useEffect, useRef, useState } from 'react'
import { Link } from 'react-router-dom'
import { User } from '@wasp/auth/types'
import api from '@wasp/api'
import { useSocket } from '@wasp/useSocket'

async function fetchCustomRoute() {
  const res = await api.get('/foo/bar')
  console.log(res.data)
}

export const ProfilePage = ({
  user: { email, isEmailVerified },
}: {
  user: User
}) => {
  const [messages, setMessages] = useState<{ id: number, username: string, text: string }[]>([]);
  const [isConnected, socket] = useSocket()
  const inputRef = useRef<HTMLInputElement>(null)

  useEffect(() => {
    fetchCustomRoute()

    socket.on('chatMessage', logMessage)

    return () => {
      socket.off('chatMessage', logMessage)
    }
  }, [])

  function logMessage(msg: { id: number, username: string, text: string }) {
    setMessages((priorMessages) => [msg, ...priorMessages])
  }

  function handleSubmit(e) {
    e.preventDefault()

    if (inputRef.current !== null) {
      socket.emit('chatMessage', inputRef.current.value)
      inputRef.current.value = ''
    }
  }

  const messageList = messages.map((msg) => <li key={msg.id}><em>{msg.username}</em>: {msg.text}</li>)
  const connectionIcon = isConnected ? '🟢' : '🔴'

  return (
    <>
      <h2>Profile page</h2>
      <div>
        Hello <strong>{email}</strong>! Your status is{' '}
        <strong>{isEmailVerified ? 'verfied' : 'unverified'}</strong>.
      </div>
      <br />
      <Link to="/">Go to dashboard</Link>
      <div>
        <form onSubmit={handleSubmit}>
          <div className="flex space-x-4 place-items-center">
            <div>{connectionIcon}</div>
            <div><input type="text" ref={inputRef} /></div>
            <div><button className="btn btn-primary" type="submit">Submit</button></div>
          </div>
        </form>
        <ul>{messageList}</ul>
      </div>
    </>
  )
}
