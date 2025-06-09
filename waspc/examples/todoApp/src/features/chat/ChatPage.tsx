import React, { useState } from "react";
import {
  useSocket,
  useSocketListener,
  type ServerToClientPayload,
} from "wasp/client/webSocket";
import { cn } from "../../cn";
import { Button } from "../../components/Button";
import { FeatureContainer } from "../../components/FeatureContainer";
import { Input } from "../../components/Input";

export const ChatPage = () => {
  const [messages, setMessages] = useState<
    ServerToClientPayload<"chatMessage">[]
  >([]);
  const { socket, isConnected } = useSocket();
  const [text, setText] = useState("");

  useSocketListener("chatMessage", (msg) =>
    setMessages((priorMessages) => [...priorMessages, msg]),
  );

  function handleSubmit(e: React.FormEvent<HTMLFormElement>) {
    e.preventDefault();

    socket.emit("chatMessage", text);
    setText("");
  }

  return (
    <FeatureContainer>
      <div className="space-y-4">
        <div className="flex justify-between items-center">
          <h2 className="feature-title">Chat Room</h2>
          <div className="flex items-center gap-2">
            <div
              className={cn(
                "h-3 w-3 rounded-full",
                isConnected ? "bg-green-500" : "bg-red-500",
              )}
            ></div>
            <span className="text-sm text-gray-600">
              {isConnected ? "Connected" : "Disconnected"}
            </span>
          </div>
        </div>
        <div className="card">
          <div className="bg-gray-50 border border-gray-200 rounded-lg p-4 h-80 overflow-y-auto mb-4">
            {messages.length > 0 ? (
              <div className="space-y-3">
                {messages.map((msg) => (
                  <div
                    key={msg.id}
                    className="bg-white rounded-lg p-3 shadow-sm border border-gray-100"
                    data-testid="message"
                  >
                    <div className="flex items-center justify-between mb-1">
                      <span className="font-medium text-primary-600 text-sm">
                        {msg.username}
                      </span>
                      <span className="text-xs text-gray-400">
                        {new Date().toLocaleTimeString()}
                      </span>
                    </div>
                    <p className="text-gray-900">{msg.text}</p>
                  </div>
                ))}
              </div>
            ) : (
              <div className="h-full flex items-center justify-center">
                <div className="text-center">
                  <p className="text-gray-500">
                    No messages yet. Test the chat by sending a message.
                  </p>
                </div>
              </div>
            )}
          </div>

          <form onSubmit={handleSubmit}>
            <div className="flex gap-2">
              <Input
                placeholder="Type your message..."
                containerClassName="flex-1"
                value={text}
                onChange={(e) => setText(e.target.value)}
                required
              />
              <Button type="submit" variant="primary">
                Send
              </Button>
            </div>
          </form>
        </div>
      </div>
    </FeatureContainer>
  );
};
