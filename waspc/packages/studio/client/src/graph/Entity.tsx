import { Handle, NodeProps, Position } from "reactflow";

export const EntityNode = ({
  data,
  isConnectable,
  targetPosition = Position.Left,
  sourcePosition = Position.Right,
}: NodeProps) => (
  <div
    className={`
    py-3 px-6 rounded bg-yellow-900 text-white text-center
    ${data.isUserEntity ? "border-3 border-yellow-300" : ""}
  `}
  >
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
    <div
      className={`
    text-xs bg-yellow-300 text-yellow-900 rounded px-1 absolute -top-1 left-1/2 -translate-x-1/2 flex items-center
    `}
    >
      <span className="mr-1">Entity</span>
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
          d="M15.75 6a3.75 3.75 0 11-7.5 0 3.75 3.75 0 017.5 0zM4.501 20.118a7.5 7.5 0 0114.998 0A17.933 17.933 0 0112 21.75c-2.676 0-5.216-.584-7.499-1.632z"
        />
      </svg>
    </div>
    <div className="font-bold">{data?.label}</div>
    <Handle
      type="target"
      position={targetPosition}
      isConnectable={isConnectable}
    />
  </div>
);
