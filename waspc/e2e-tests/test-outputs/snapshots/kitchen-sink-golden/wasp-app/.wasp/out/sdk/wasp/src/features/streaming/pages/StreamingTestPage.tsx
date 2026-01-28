import { useEffect, useState } from "react";
import { config } from "wasp/client";
import { FeatureContainer } from "../../../components/FeatureContainer";

export const StreamingTestPage = () => {
  const { response } = useTextStream("/api/streaming-test");
  return (
    <FeatureContainer>
      <div className="space-y-4">
        <h2 className="feature-title">Streaming Demo</h2>
        <div className="card">
          <p className="max-w-2xl">{response}</p>
        </div>
      </div>
    </FeatureContainer>
  );
};

function useTextStream(path: string): { response: string } {
  const [response, setResponse] = useState("");
  useEffect(() => {
    const controller = new AbortController();
    fetchStream(
      path,
      (chunk) => {
        setResponse((prev) => prev + chunk);
      },
      controller,
    );

    return () => {
      controller.abort();
    };
  }, []);

  return {
    response,
  };
}

async function fetchStream(
  path: string,
  onData: (data: string) => void,
  controller: AbortController,
) {
  const response = await fetch(config.apiUrl + path, {
    signal: controller.signal,
  });

  if (response.body === null) {
    throw new Error("Stream body is null");
  }

  const reader = response.body.pipeThrough(new TextDecoderStream()).getReader();
  while (true) {
    const { done, value } = await reader.read();
    if (done) {
      return;
    }
    onData(value.toString());
  }
}
