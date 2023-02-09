const apiUrl = process.env.REACT_APP_API_URL?.replace(/\/$/, '') || 'http://localhost:3001';

const config = {
  apiUrl,
}

export default config
