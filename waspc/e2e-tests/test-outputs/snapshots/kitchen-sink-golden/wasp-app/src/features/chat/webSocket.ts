import { v4 as uuidv4 } from "uuid";
import {
  broadcast,
  defineWebSocket,
  type WaspSocketPeer,
} from "wasp/server/webSocket";

export const chatWebSocket = defineWebSocket<
  ClientToServerEvents,
  ServerToClientEvents
>({
  open(peer) {
    console.log("a user connected: ", getUsername(peer));
  },

  events: {
    chatMessage(peer, msg) {
      console.log("message: ", msg);
      broadcast("chatMessage", {
        id: uuidv4(),
        username: getUsername(peer),
        text: msg,
      });
    },
  },
});

function getUsername(peer: WaspSocketPeer<ServerToClientEvents>): string {
  return peer.data.user?.getFirstProviderUserId() ?? "Unknown";
}

interface ServerToClientEvents {
  chatMessage: (msg: { id: string; username: string; text: string }) => void;
}
interface ClientToServerEvents {
  chatMessage: (msg: string) => void;
}
