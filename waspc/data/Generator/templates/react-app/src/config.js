
const config = {
  apiUrl: import.meta.env.REACT_APP_API_URL?.replace(/\/$/, '') || 'http://localhost:3001'
}

export default config
