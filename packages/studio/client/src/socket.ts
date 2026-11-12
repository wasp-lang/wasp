import { useEffect, useState } from "react";
import { io } from "socket.io-client";
import { Data } from "./types";

export const socket = io("http://localhost:4000");

export function useSocket() {
  const [isConnected, setIsConnected] = useState(false);

  const [data, setData] = useState<Data | null>(null);

  function onData(data: string) {
    try {
      setData(JSON.parse(data) as Data);
    } catch (e: unknown) {
      console.error(e);
    }
  }

  function onConnect() {
    setIsConnected(true);
  }

  function onDisconnect() {
    setIsConnected(false);
  }

  useEffect(() => {
    socket.on("connect", onConnect);
    socket.on("disconnect", onDisconnect);
    socket.on("data", onData);
    return () => {
      socket.off("data", onData);
      socket.off("connect", onConnect);
      socket.off("disconnect", onDisconnect);
    };
  }, []);

  return {
    data,
    socket,
    isConnected,
  };
}
