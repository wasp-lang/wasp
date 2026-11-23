import tailwindcss from "@tailwindcss/vite";
import react from "@vitejs/plugin-react-swc";
import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [react(), tailwindcss()],
  server: {
    // We need to serve two things: the frontend, and the socket it gets the
    // app data from. In production, the Studio CLI spins up a server that
    // serves both. In development, Vite is in charge of the frontend, so we ask
    // it to proxy the socket to the Studio CLI's server.
    proxy: {
      "/socket.io": { target: "http://localhost:4000", ws: true },
    },
  },
});
