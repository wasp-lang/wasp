import React, { useEffect, useState } from 'react'
import Clock from 'react-clock'
import 'react-clock/dist/Clock.css'

export default () => {
  const [time, setTime] = useState(new Date())

  useEffect(() => {
    const interval = setInterval(() => setTime(new Date()), 1000)
    return () => clearInterval(interval)
  }, [])

  return (
    <div style={{ display: 'flex' }}>
      <Clock value={time} />
      <Clock value={new Date(time.getTime() + 60 * 60000)} />
    </div>
  )
}
