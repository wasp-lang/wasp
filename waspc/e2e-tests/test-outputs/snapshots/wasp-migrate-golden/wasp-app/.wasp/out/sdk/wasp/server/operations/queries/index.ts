
import { prisma } from '../../index.js'
import { getServerOperation } from '../../runtime.js'
import {
  type UnauthenticatedOperationFor,
  createUnauthenticatedOperation,
} from '../wrappers.js'
import type { FromRegisterPath } from '../../../types/register.js'
import type {
} from './types.js'
