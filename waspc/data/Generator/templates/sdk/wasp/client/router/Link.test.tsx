import { useState } from 'react'
import { render, screen } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { MemoryRouter } from 'react-router-dom'
import { describe, it, expect } from 'vitest'
import { Link } from './Link'

describe('Link', () => {
  it('updates href when search prop changes', async () => {
    function Harness() {
      const [intent, setIntent] = useState<'apply' | 'post'>('apply')
      return (
        <>
          <Link to="/auth" search={{ intent }} data-testid="link">Auth</Link>
          <button onClick={() => setIntent(intent === 'apply' ? 'post' : 'apply')}>Toggle</button>
        </>
      )
    }

    render(<MemoryRouter><Harness /></MemoryRouter>)
    const link = screen.getByTestId('link')

    expect(link).toHaveAttribute('href', '/auth?intent=apply')
    await userEvent.setup().click(screen.getByText('Toggle'))
    expect(link).toHaveAttribute('href', '/auth?intent=post')
  })

  it('updates href when hash prop changes', async () => {
    function Harness() {
      const [hash, setHash] = useState('top')
      return (
        <>
          <Link to="/page" hash={hash} data-testid="link">Page</Link>
          <button onClick={() => setHash(hash === 'top' ? 'bottom' : 'top')}>Toggle</button>
        </>
      )
    }

    render(<MemoryRouter><Harness /></MemoryRouter>)
    const link = screen.getByTestId('link')

    expect(link).toHaveAttribute('href', '/page#top')
    await userEvent.setup().click(screen.getByText('Toggle'))
    expect(link).toHaveAttribute('href', '/page#bottom')
  })
})
