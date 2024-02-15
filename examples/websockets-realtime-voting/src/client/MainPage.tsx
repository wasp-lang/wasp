import { type ServerToClientPayload, useSocket, useSocketListener } from "wasp/client/webSocket";
import { useAuth } from "wasp/client/auth";
import { useState, useMemo, useEffect } from "react";
import { Button, Card } from "flowbite-react";
import { getUsername } from 'wasp/auth'

const MainPage = () => {
  const { data: user } = useAuth();
  const [poll, setPoll] = useState<ServerToClientPayload<"updateState"> | null>(
    null
  );
  const totalVotes = useMemo(() => {
    return (
      poll?.options.reduce((acc, option) => acc + option.votes.length, 0) ?? 0
    );
  }, [poll]);

  const { socket } = useSocket();

  const username = user ? getUsername(user) : null;

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
    <div className="w-full max-w-2xl mx-auto p-8">
      <h1 className="text-2xl font-bold">{poll?.question ?? "Loading..."}</h1>
      {poll && (
        <p className="leading-relaxed text-gray-500">
          Cast your vote for one of the options.
        </p>
      )}
      {poll && (
        <div className="mt-4 flex flex-col gap-4">
          {poll.options.map((option) => (
            <Card key={option.id} className="relative transition-all duration-300 min-h-[130px]">
              <div className="z-10">
                <div className="mb-2">
                  <h2 className="text-xl font-semibold">{option.text}</h2>
                  <p className="text-gray-700">{option.description}</p>
                </div>
                <div className="absolute bottom-5 right-5">
                  {username && !option.votes.includes(username) ? (
                    <Button onClick={() => handleVote(option.id)}>Vote</Button>
                  ) : (
                    <Button disabled>Voted</Button>
                  )}
                  {!user}
                </div>
                {option.votes.length > 0 && (
                  <div className="mt-2 flex gap-2 flex-wrap max-w-[75%]">
                    {option.votes.map((vote) => (
                      <div
                        key={vote}
                        className="py-1 px-3 bg-gray-100 rounded-lg flex items-center justify-center shadow text-sm"
                      >
                        <div className="w-2 h-2 bg-green-500 rounded-full mr-2"></div>
                        <div className="text-gray-700">{vote}</div>
                      </div>
                    ))}
                  </div>
                )}
              </div>
              <div className="absolute top-5 right-5 p-2 text-sm font-semibold bg-gray-100 rounded-lg z-10">
                {option.votes.length} / {totalVotes}
              </div>
              <div
                className="absolute inset-0 bg-gradient-to-r from-yellow-400 to-orange-500 opacity-75 rounded-lg transition-all duration-300"
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
