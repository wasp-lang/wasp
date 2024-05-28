import { useLocation } from 'react-router-dom'

export function CatchAllPage() {
  const location = useLocation()

  return (
    <div className="flex flex-col items-center justify-center min-h-80">
      <div className="space-y-2 text-center">
        <h1 className="text-2xl font-bold mb-4">Not found</h1>
        <p className="text-gray-500">
          We couldn't find anything at the{' '}
          <code className="text-gray-700 font-mono bg-gray-200 rounded px-2">
            {location.pathname}
          </code>{' '}
          location.
        </p>
      </div>
    </div>
  )
}
