import React, { useState, useEffect } from "react";
import { useQuery } from "@wasp/queries";
import getIdeas from "@wasp/queries/getIdeas";
import createIdea from "@wasp/actions/createIdea";
import toggleAgreementFn from "@wasp/actions/toggleAgreement";
import { useSocket } from "@wasp/webSocket";
import { useQueryClient } from "@tanstack/react-query";


const MainPage = ({ user }: any) => {
  const { socket, isConnected } = useSocket();
  const [ideaText, setIdeaText] = useState("");
  const { data: ideas, isLoading } = useQuery(getIdeas, undefined, {
    refetchOnWindowFocus: false,
  });
  const queryClient = useQueryClient();
  async function sendIdea(event: any) {
    event.preventDefault();
    await createIdea({ idea: ideaText });
    socket.emit("ideaCreated");
    setIdeaText("");
  }
  async function toggleAgreement(idea: any) {
    await toggleAgreementFn({ id: idea.id });
    socket.emit("agreementToggled");
  }
  function getClassName(ide: any) {
    let base = "idea";
    if (idea.user.username === user.username) {
      base += " mine";
    }
    if (idea.agreedUsers.includes(user.username)) {
      base += " agreed";
    }
    return base;
  }

  useEffect(() => {
    socket.on("ideaCreated", () => {
      queryClient.invalidateQueries(getIdeas.queryCacheKey);
    });
    socket.on("agreementToggled", () => {
      queryClient.invalidateQueries(getIdeas.queryCacheKey);
    });
    return () => {
      socket.off("ideaCreated");
      socket.off("agreementToggled");
    };
  }, []);

  return (
    <main>
      <header>
        <p>WebSocket Status: {isConnected ? "🟢" : "🔴"}</p>
      </header>
      <h1>Agreement App</h1>
      <p>
        Rules: You can propose your ideas, but you can only agree to others'
        ideas.
      </p>
      <div className="ideas">
        {isLoading && <p>Loading...</p>}
        {ideas &&
          ideas.map((idea) => (
            <div key={idea.id} className={getClassName(idea)}>
              <p>{idea.user.username}</p>
              <h3>{idea.idea}</h3>
              <button onClick={() => toggleAgreement(idea)}>
                {idea.agreedUsers.includes(user.username) ? "Agreed" : "Agree"}
              </button>
              {idea.agreedUsers.length > 0 && (
                <small>
                  Agreed:{" "}
                  {idea.agreedUsers.map((username) => username).join(", ")}
                </small>
              )}
            </div>
          ))}
        {ideas && ideas.length === 0 && <p className="idea">No ideas yet.</p>}
      </div>
      <form onSubmit={sendIdea}>
        <label htmlFor="idea">Idea</label>
        <input
          type="text"
          value={ideaText}
          onChange={(event) => setIdeaText(event.target.value)}
          id="idea"
        />
        <button type="submit">Send idea</button>
      </form>
    </main>
  );
};
export default MainPage;
