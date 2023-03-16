/// <reference types="vitest" />
import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    port: 3000,
    host: '0.0.0.0',
  },
  envPrefix: 'REACT_APP_',
  build: {
    outDir: 'build',
  },
  test: {
    environment: 'jsdom'
  },
})
