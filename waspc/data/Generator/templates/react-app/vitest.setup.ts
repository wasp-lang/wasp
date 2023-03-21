import matchers from '@testing-library/jest-dom/matchers'
import { expect } from 'vitest'

import { initCallbacks } from './vitest.helpers.js'

expect.extend(matchers)

initCallbacks()
