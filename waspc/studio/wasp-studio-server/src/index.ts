import * as fs from "fs";

import Fastify from "fastify";
import FastifySocketIO from "fastify-socket.io";
import cors from "@fastify/cors";

const fastify = Fastify({
  logger: true,
});

fastify.register(FastifySocketIO, {
  cors: {
    origin: "*",
  },
});
fastify.register(cors, {
  origin: true,
});

fastify.get("/", async function handler(request, reply) {
  return { hello: "world" };
});

const file = new URL("../../data.json", import.meta.url);
let data = fs.readFileSync(file, "utf8");
fs.watch(file, () => {
  data = fs.readFileSync(file, "utf8");
  fastify.io.emit("data", data);
});

fastify.ready((err) => {
  if (err) throw err;
  fastify.io.on("connection", (socket) => {
    console.log("Client connected");
    socket.emit("data", data);
    socket.on("disconnect", () => {
      console.log("Client disconnected");
    });
  });
});

try {
  await fastify.listen({ port: 4000 });
} catch (err) {
  fastify.log.error(err);
  process.exit(1);
}
