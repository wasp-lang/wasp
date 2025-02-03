import { loadEnv } from 'vite'

const mode = process.env.NODE_ENV || 'development'
const env = loadEnv(mode, process.cwd(), 'VITE_')

const apiPrefix =
  env.VITE_WASP_API_PREFIX?.trim() || '/api'

const apiPrefixWithoutSlash = apiPrefix.replace(/^\//, '')
const serverUrl = env.VITE_WASP_SERVER_URL?.trim() || 'http://localhost:3001'

export default {
  server: {
    open: true,
    proxy: {
      [apiPrefix]: {
        target: serverUrl,
        changeOrigin: true,
        rewrite: (path) =>
          path.replace(new RegExp(`^/${apiPrefixWithoutSlash}`), ''),
        ws: true,
      },
      '/socket.io/': {
        target: serverUrl,
        changeOrigin: true,
        ws: true,
      },
    },
  },
}


// import { defineConfig } from 'vite'

// export default defineConfig({
//   server: {
//     open: false,
//   },
// })
