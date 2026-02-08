import { defineConfig } from 'vite';
import { wasp } from 'wasp/client/vite';

export default defineConfig({
  server: {
    open: false,
  },
  plugins: [wasp()],
});
