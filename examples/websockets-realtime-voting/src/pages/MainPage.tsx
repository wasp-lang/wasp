import { Button, Card } from "flowbite-react";
import { useEffect, useMemo, useState } from "react";
import { useAuth } from "wasp/client/auth";
import {
  type ServerToClientPayload,
  useSocket,
  useSocketListener,
} from "wasp/client/webSocket";

const MainPage = () => {
  const { data: user } = useAuth();
  const [poll, setPoll] = useState<ServerToClientPayload<"updateState"> | null>(
    null,
  );
  const totalVotes = useMemo(() => {
    return (
      poll?.options.reduce((acc, option) => acc + option.votes.length, 0) ?? 0
    );
  }, [poll]);

  const { socket } = useSocket();

  const username = user?.getFirstProviderUserId();

  const TRUNCATE_SIZE = 7;

  useSocketListener("updateState", (newState) => {
    setPoll(newState);
  });

  useEffect(() => {
    socket.emit("askForStateUpdate");
  }, []);

  function handleVote(optionId: number) {
    socket.emit("vote", optionId);
  }

  return (
    <div className="mx-auto w-full max-w-2xl p-8">
      <h1 className="text-2xl font-bold">{poll?.question ?? "Loading..."}</h1>
      {poll && (
        <p className="leading-relaxed text-gray-500">
          Cast your vote for one of the options.
        </p>
      )}
      {poll && (
        <div className="mt-4 flex flex-col gap-4">
          {poll.options.map((option) => (
            <Card
              key={option.id}
              className="card relative min-h-[130px] transition-all duration-300"
            >
              <div className="z-10">
                <div className="mb-2">
                  <h2 className="text-xl font-semibold">{option.text}</h2>
                  <p className="text-gray-700">{option.description}</p>
                </div>
                <div className="absolute bottom-5 right-5">
                  {username && !option.votes.includes(username) ? (
                    <Button color="cyan" onClick={() => handleVote(option.id)}>
                      Vote
                    </Button>
                  ) : (
                    <Button color="cyan" disabled>
                      Voted
                    </Button>
                  )}
                  {!user}
                </div>
                {option.votes.length > 0 && (
                  <div className="mt-2 flex max-w-[75%] flex-wrap gap-2">
                    {option.votes.map((username, idx) => {
                      if (idx > TRUNCATE_SIZE) {
                        return undefined;
                      }
                      return (
                        <div
                          key={username}
                          className="username flex items-center justify-center rounded-lg bg-gray-100 px-3 py-1 text-sm shadow"
                        >
                          <div className="mr-2 h-2 w-2 rounded-full bg-green-500"></div>
                          <div className="text-gray-700">{username}</div>
                        </div>
                      );
                    })}
                    {option.votes.length > TRUNCATE_SIZE + 2 && (
                      <div className="text-gray-700">{`...`}</div>
                    )}
                    {option.votes.length > TRUNCATE_SIZE + 1 && (
                      <div className="flex items-center justify-center rounded-lg bg-gray-100 px-3 py-1 text-sm shadow">
                        <div className="mr-2 h-2 w-2 rounded-full bg-green-500"></div>
                        <div className="text-gray-700">
                          {option.votes[option.votes.length - 1]}
                        </div>
                      </div>
                    )}
                  </div>
                )}
              </div>
              <div className="votes-count absolute right-5 top-5 z-10 rounded-lg bg-gray-100 p-2 text-sm font-semibold">
                {option.votes.length} / {totalVotes}
              </div>
              <div
                className="absolute inset-0 rounded-lg bg-gradient-to-r from-yellow-400 to-orange-500 opacity-75 transition-all duration-300"
                style={{
                  width: `${
                    totalVotes > 0
                      ? (option.votes.length / totalVotes) * 100
                      : 0
                  }%`,
                }}
              ></div>
            </Card>
          ))}
        </div>
      )}
    </div>
  );
};
export default MainPage;
