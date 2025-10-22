import { useDisclosure } from "@nextui-org/react";
import { useState } from "react";
import { Handle, NodeProps, Position } from "reactflow";
import { AddEntityModal } from "./AddEntityModal";

export const AppNode = ({
  data,
  isConnectable,
  targetPosition = Position.Left,
  sourcePosition = Position.Right,
}: NodeProps) => {
  const [isHovered, setIsHovered] = useState(false);
  const addEntityModal = useDisclosure();

  const handleAddRoute = () => {
    data.onAddRoute?.();
  };

  return (
    <div
      className="relative"
      onMouseEnter={() => setIsHovered(true)}
      onMouseLeave={() => setIsHovered(false)}
    >
      <div className="rounded bg-cyan-900 px-6 py-3 text-center text-white">
        <Handle
          type="target"
          position={targetPosition}
          isConnectable={isConnectable}
        />
        <div className="absolute -top-1 left-1/2 flex -translate-x-1/2 items-center rounded bg-cyan-300 px-1 text-xs text-cyan-900">
          <span className="mr-1">App</span>
          <svg
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 24 24"
            strokeWidth={1.5}
            stroke="currentColor"
            className="h-3 w-3"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              d="M8.25 3v1.5M4.5 8.25H3m18 0h-1.5M4.5 12H3m18 0h-1.5m-15 3.75H3m18 0h-1.5M8.25 19.5V21M12 3v1.5m0 15V21m3.75-18v1.5m0 15V21m-9-1.5h10.5a2.25 2.25 0 002.25-2.25V6.75a2.25 2.25 0 00-2.25-2.25H6.75A2.25 2.25 0 004.5 6.75v10.5a2.25 2.25 0 002.25 2.25zm.75-12h9v9h-9v-9z"
            />
          </svg>
        </div>
        <div className="font-bold">{data?.label}</div>
        <Handle
          type="source"
          position={sourcePosition}
          isConnectable={isConnectable}
        />
        <div className="mt-2 flex flex-col items-center justify-center gap-2">
          <div className="bg-foreground text-background rounded px-1 text-xs">
            <span>{data.db?.system || "SQLite"}</span>
          </div>
          {data.auth &&
            data.auth.methods.map((method: string) => (
              <div
                className="bg-foreground text-background rounded px-1 text-xs"
                key={method}
              >
                <span>Auth: {method}</span>
              </div>
            ))}
        </div>
      </div>
      {isHovered && (
        <div className="absolute -bottom-8 left-1/2 -translate-x-1/2 flex gap-2 bg-cyan-800 rounded px-2 py-1 whitespace-nowrap">
          <button
            onClick={addEntityModal.onOpenChange}
            className="text-xs bg-yellow-600 hover:bg-yellow-500 text-white rounded px-2 py-1 transition-colors"
            title="Add new entity"
          >
            + Entity
          </button>
          <AddEntityModal
            isOpen={addEntityModal.isOpen}
            onClose={() => addEntityModal.onOpenChange()}
            onEntityAdd={data.onAddEntity}
          />
          <button
            onClick={handleAddRoute}
            className="text-xs bg-blue-600 hover:bg-blue-500 text-white rounded px-2 py-1 transition-colors"
            title="Add new route"
          >
            + Route
          </button>
        </div>
      )}
    </div>
  );
};
