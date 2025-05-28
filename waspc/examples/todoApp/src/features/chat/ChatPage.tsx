import React, { useRef, useState } from "react";
import {
  useSocket,
  useSocketListener,
  type ServerToClientPayload,
} from "wasp/client/webSocket";
import { Button } from "../../components/Button";
import { Input } from "../../components/Input";

export const ChatPage = () => {
  const [messages, setMessages] = useState<
    ServerToClientPayload<"chatMessage">[]
  >([]);
  const { socket, isConnected } = useSocket();
  const inputRef = useRef<HTMLInputElement>(null);

  useSocketListener("chatMessage", (msg) =>
    setMessages((priorMessages) => [...priorMessages, msg]),
  );

  function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault();

    if (inputRef.current !== null) {
      socket.emit("chatMessage", inputRef.current.value);
      inputRef.current.value = "";
    }
  }

  return (
    <div className="max-w-2xl mx-auto my-8">
      <div className="flex justify-between items-center gap-2 mb-4">
        <h2 className="text-2xl font-bold">Chat Room</h2>
        <div className="flex items-center gap-2">
          <div
            className={`h-3 w-3 rounded-full ${
              isConnected ? "bg-green-500" : "bg-red-500"
            }`}
          ></div>
          <span className="text-sm text-gray-500">
            {isConnected ? "Connected" : "Disconnected"}
          </span>
        </div>
      </div>

      <div className="bg-gray-50 border border-gray-200 rounded-lg shadow-sm mb-4 p-4 h-80 overflow-y-auto">
        {messages.length > 0 ? (
          <ul className="space-y-2">
            {messages.map((msg) => (
              <li
                key={msg.id}
                className="p-2 bg-white rounded-lg shadow-sm border border-gray-100"
                data-testid="message"
              >
                <span className="font-medium text-blue-600">
                  {msg.username}
                </span>
                <p className="mt-1">{msg.text}</p>
              </li>
            ))}
          </ul>
        ) : (
          <div className="h-full flex items-center justify-center">
            <p className="text-gray-500">
              No messages yet. Start the conversation!
            </p>
          </div>
        )}
      </div>

      <form onSubmit={handleSubmit} className="mt-4">
        <div className="flex gap-2">
          <Input
            ref={inputRef}
            placeholder="Type your message..."
            containerClassName="w-full"
            inputClassName="w-full"
            required
          />
          <Button type="submit" variant="primary">
            Send
          </Button>
        </div>
      </form>
    </div>
  );
};
