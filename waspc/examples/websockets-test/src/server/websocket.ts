import { WebSocketDefinition } from "@wasp/webSocket";
import { createIdea } from "./ideas";

type ClientToServerEvents = {
  ideaCreated: () => void;
  agreementToggled: () => void;
};

type ServerToClientEvents = {
  ideaCreated: (data: { username: string }) => void;
  agreementToggled: (data: { username: string }) => void;
};

export const webSocketFn: WebSocketDefinition<
  ClientToServerEvents,
  ServerToClientEvents
> = async (io, context) => {
  io.on("connection", (socket) => {
    const user = socket.data.user;
    if (!user) {
      return;
    }
    socket.on("ideaCreated", () => {
      io.emit("ideaCreated", { username: user.username });
    });

    socket.on("agreementToggled", () => {
      io.emit("agreementToggled", { username: user.username });
    });
    // socket.on('createIdea', (event) => {

    // });
    // socket.on('toggleAgreement', (event) => {
    //     console.log(event);
    //     // io.emit('agreementToggled', {});
    // })
  });
};
