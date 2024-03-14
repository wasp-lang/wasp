import { config } from 'wasp/client'
import { useState, useEffect } from 'react'
import './Main.css'

const MainPage = () => {
  const { response } = useTextStream('/api/streaming-test')
  return (
    <div className="container">
      <main>
        <h1>Streaming Demo</h1>
        <p
          style={{
            maxWidth: '600px',
          }}
        >
          {response}
        </p>
      </main>
    </div>
  )
}
export default MainPage

function useTextStream(path) {
  const [response, setResponse] = useState('')
  useEffect(() => {
    const controller = new AbortController()
    fetchStream(
      path,
      (chunk) => {
        setResponse((prev) => prev + chunk)
      },
      controller
    )
    return () => {
      controller.abort()
    }
  }, [])

  return {
    response,
  }
}

async function fetchStream(path, onData, controller) {
  const response = await fetch(config.apiUrl + path, {
    signal: controller.signal,
  })
  const reader = response.body.pipeThrough(new TextDecoderStream()).getReader()
  while (true) {
    const { done, value } = await reader.read()
    if (done) {
      return
    }
    onData(value.toString())
  }
}
