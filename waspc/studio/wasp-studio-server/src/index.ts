import * as fs from "fs";

import Fastify from "fastify";
import FastifySocketIO from "fastify-socket.io";
import FastifyStatic from "@fastify/static";
import cors from "@fastify/cors";
import { Command } from "commander";

const program = new Command();
// Parse --file option
program
  .requiredOption("-f, --file <path>", "Path to data file")
  .requiredOption("-p, --public <path>", "Path to the public folder")
  .parse(process.argv);

const options = program.opts<{
  file: string;
  public: string;
}>();

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
fastify.register(FastifyStatic, {
  root: new URL(options.public, import.meta.url).pathname,
});

const pathToDataFile = new URL(options.file, import.meta.url);
function readFile() {
  return fs.readFileSync(pathToDataFile, "utf8");
}

let data = readFile();
fs.watch(pathToDataFile, () => {
  data = readFile();
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
