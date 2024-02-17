import { describe, it, expect } from 'vitest'

import { getTotalTaskCountMessage } from './helpers'

describe('helpers', () => {
  it('not loaded yet -> empty string', () => {
    expect(getTotalTaskCountMessage()).toBe('')
  })
  it('no tasks -> 0 tasks message', () => {
    expect(getTotalTaskCountMessage(0)).toBe('No tasks created, yet.')
  })
  it('one task -> 1 task message', () => {
    expect(getTotalTaskCountMessage(1)).toBe('There is just one task.')
  })
  it('multiple tasks -> default message', () => {
    expect(getTotalTaskCountMessage(2)).toBe(
      'There are 2 tasks created so far.'
    )
  })
})
