import { page, route } from "@wasp.sh/spec";

import { ChatPage } from "./pages/ChatPage" with { type: "ref" };
import { webSocketFn } from "./webSocket" with { type: "ref" };

export const webSocket = {
  fn: webSocketFn,
};

export const chat = [
  route("ChatRoute", "/chat", page(ChatPage, { authRequired: true })),
];
