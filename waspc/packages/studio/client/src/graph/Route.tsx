import { Handle, NodeProps, Position } from "reactflow";

export const RouteNode = ({
  data,
  isConnectable,
  sourcePosition = Position.Left,
  targetPosition = Position.Right,
}: NodeProps) => (
  <div className="py-3 px-6 rounded bg-rose-900 text-white text-center">
    <Handle
      type="target"
      position={targetPosition}
      isConnectable={isConnectable}
    />
    <div className="text-xs bg-rose-300 text-rose-900 rounded px-1 absolute -top-1 left-1/2 -translate-x-1/2 flex items-center">
      <span className="mr-1">Route</span>
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
          d="M15.042 21.672L13.684 16.6m0 0l-2.51 2.225.569-9.47 5.227 7.917-3.286-.672zM12 2.25V4.5m5.834.166l-1.591 1.591M20.25 10.5H18M7.757 14.743l-1.59 1.59M6 10.5H3.75m4.007-4.243l-1.59-1.59"
        />
      </svg>
    </div>
    <div className="font-bold">{data?.label}</div>
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
  </div>
);
