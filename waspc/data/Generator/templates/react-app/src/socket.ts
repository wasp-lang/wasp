import { io } from 'socket.io-client'
import config from './config'

// TODO: In the future, it would be nice if users could configure this somehow.
export const socket = io(config.apiUrl)
