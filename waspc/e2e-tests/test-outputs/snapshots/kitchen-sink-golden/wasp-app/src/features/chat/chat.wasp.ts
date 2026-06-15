import { page, route, type Spec, type WebSocket } from "@wasp.sh/spec";

import { ChatPage } from "./pages/ChatPage" with { type: "ref" };
import { chatWebSocket } from "./webSocket" with { type: "ref" };

export const webSocket: WebSocket = {
  fn: chatWebSocket,
};

export const chatSpec: Spec = [
  route("ChatRoute", "/chat", page(ChatPage, { authRequired: true })),
];
