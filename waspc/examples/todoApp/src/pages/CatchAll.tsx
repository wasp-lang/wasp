import { useLocation } from 'react-router-dom'

export function CatchAllPage() {
  const location = useLocation()

  return (
    <div className='flex min-h-80 flex-col items-center justify-center'>
      <div className='space-y-2 text-center'>
        <h1 className='mb-4 text-2xl font-bold'>Not found</h1>
        <p className='text-gray-500'>
          We couldn't find anything at the{' '}
          <code className='rounded bg-gray-200 px-2 font-mono text-gray-700'>
            {location.pathname}
          </code>{' '}
          location.
        </p>
      </div>
    </div>
  )
}
