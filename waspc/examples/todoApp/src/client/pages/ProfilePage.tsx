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
  const [messages, setMessages] = useState<{ id: number, text: string }[]>([]);
  const [isConnected, socket] = useSocket()
  const inputRef = useRef<HTMLInputElement>(null)

  function logMessage(msg: { id: number, text: string }) {
    setMessages((priorMessages) => [msg, ...priorMessages])
  }

  function handleClick() {
    if (inputRef.current !== null) {
      socket.emit('chat message', inputRef.current.value)
      inputRef.current.value = ''
    }
  }

  useEffect(() => {
    fetchCustomRoute()

    socket.on('chat message', logMessage)

    return () => {
      socket.off('chat message', logMessage)
    }
  }, [])

  const messageList = messages.map((msg) => <li key={msg.id}>{msg.text}</li>)

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
        <p>Connected: {`${isConnected}`}</p>
        <input type="text" ref={inputRef} />
        <button type="submit" onClick={handleClick}>Submit</button>
        <ul>{messageList}</ul>
      </div>
    </>
  )
}
