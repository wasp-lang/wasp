import axios from 'axios'
import config from './config'

const api = axios.create({
  baseURL: config.apiUrl,
  timeout: parseInt(process.env.REACT_APP_AXIOS_TIMEOUT) // Undefined is fine, will use default value.
})

let authToken = null

export const setAuthToken = (token) => {
  if (typeof token !== 'string') {
    throw Error(`Token must be a string, but it was: {${typeof token}} ${token}.`)
  }
  authToken = token
}

export const clearAuthToken = () => {
  authToken = undefined
}

// TODO(matija): add interceptor to use token when available.
