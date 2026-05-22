import { page, route, type Part, type WebSocket } from "@wasp.sh/spec";

import { ChatPage } from "./pages/ChatPage" with { type: "ref" };
import { chatWebSocket } from "./webSocket" with { type: "ref" };

export const webSocket: WebSocket = {
  fn: chatWebSocket,
};

export const chat: Part[] = [
  route("ChatRoute", "/chat", page(ChatPage, { authRequired: true })),
];
