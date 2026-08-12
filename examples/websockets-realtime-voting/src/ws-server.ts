import {
  broadcast,
  defineWebSocket,
  type WaspSocketPeer,
} from "wasp/server/webSocket";

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

export const votingWebSocket = defineWebSocket<
  ClientToServerEvents,
  ServerToClientEvents
>({
  open(peer) {
    const username = getUsername(peer);
    if (!username) {
      console.log("Socket connected without user");
      return;
    }
    console.log("Socket connected: ", username);
  },

  events: {
    askForStateUpdate(peer) {
      peer.send("updateState", poll);
    },

    vote(peer, optionId) {
      const username = getUsername(peer);
      if (!username) {
        return;
      }

      // If user has already voted, remove their vote.
      poll.options.forEach((option) => {
        option.votes = option.votes.filter(
          (votingUsername) => votingUsername !== username,
        );
      });
      // And then add their vote to the new option.
      const option = poll.options.find((o) => o.id === optionId);
      if (!option) {
        return;
      }
      option.votes.push(username);

      broadcast("updateState", poll);
    },
  },

  close(peer) {
    console.log("Socket disconnected: ", getUsername(peer) ?? "unknown");
  },
});

function getUsername(
  peer: WaspSocketPeer<ServerToClientEvents>,
): string | undefined {
  return peer.data.user?.getFirstProviderUserId() ?? undefined;
}
