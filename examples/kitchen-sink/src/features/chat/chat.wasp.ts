import { page, route, type Decl, type WebSocket } from "@wasp.sh/spec";

import { ChatPage } from "./pages/ChatPage" with { type: "ref" };
import { chatWebSocket } from "./webSocket" with { type: "ref" };

export const webSocket: WebSocket = {
  fn: chatWebSocket,
};

export const chatDecls: Decl[] = [
  route("ChatRoute", "/chat", page(ChatPage, { authRequired: true })),
];
