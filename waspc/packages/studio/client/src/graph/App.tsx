import { Handle, NodeProps, Position } from "reactflow";

export const AppNode = ({
  data,
  isConnectable,
  targetPosition = Position.Left,
  sourcePosition = Position.Right,
}: NodeProps) => (
  <div className="py-3 px-6 rounded bg-cyan-900 text-white text-center">
    <Handle
      type="target"
      position={targetPosition}
      isConnectable={isConnectable}
    />
    <div className="text-xs bg-cyan-300 text-cyan-900 rounded px-1 absolute -top-1 left-1/2 -translate-x-1/2 flex items-center">
      <span className="mr-1">App</span>
      <svg
        xmlns="http://www.w3.org/2000/svg"
        fill="none"
        viewBox="0 0 24 24"
        strokeWidth={1.5}
        stroke="currentColor"
        className="w-3 h-3"
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
    <div className="flex justify-center flex-col items-center mt-2 gap-2">
      <div className="text-xs bg-foreground text-background rounded px-1">
        <span>{data.db?.system || "SQLite"}</span>
      </div>
      {data.auth &&
        data.auth.methods.map((method: string) => (
          <div
            className="text-xs bg-foreground text-background rounded px-1"
            key={method}
          >
            <span>Auth: {method}</span>
          </div>
        ))}
    </div>
  </div>
);
