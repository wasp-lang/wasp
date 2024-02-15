import { getUsername } from "wasp/auth";
import { type WebSocketDefinition } from "wasp/server/webSocket";

type PollState = {
  question: string;
  options: {
    id: number;
    text: string;
    description: string;
    votes: string[];
  }[];
};

interface ServerToClientEvents {
  updateState: (state: PollState) => void;
}
interface ClientToServerEvents {
  vote: (optionId: number) => void;
  askForStateUpdate: () => void;
}
interface InterServerEvents {}

export const webSocketFn: WebSocketDefinition<
  ClientToServerEvents,
  ServerToClientEvents,
  InterServerEvents
> = (io, context) => {
  const poll: PollState = {
    question: "What are eating for lunch ✨ Let's order",
    options: [
      {
        id: 1,
        text: "Party Pizza Place",
        description: "Best pizza in town",
        votes: [],
      },
      {
        id: 2,
        text: "Best Burger Joint",
        description: "Best burger in town",
        votes: [],
      },
      {
        id: 3,
        text: "Sus Sushi Place",
        description: "Best sushi in town",
        votes: [],
      },
    ],
  };
  io.on("connection", (socket) => {
    if (!socket.data.user) {
      console.log("Socket connected without user");
      return;
    }

    const connectionUsername = getUsername(socket.data.user);

    console.log("Socket connected: ", connectionUsername);
    socket.on("askForStateUpdate", () => {
      socket.emit("updateState", poll);
    });

    socket.on("vote", (optionId) => {
      if (!connectionUsername) {
        return;
      }
      // If user has already voted, remove their vote.
      poll.options.forEach((option) => {
        option.votes = option.votes.filter(
          (username) => username !== connectionUsername
        );
      });
      // And then add their vote to the new option.
      const option = poll.options.find((o) => o.id === optionId);
      if (!option) {
        return;
      }
      option.votes.push(connectionUsername);
      io.emit("updateState", poll);
    });

    socket.on("disconnect", () => {
      console.log("Socket disconnected: ", connectionUsername ?? "unknown");
    });
  });
};
