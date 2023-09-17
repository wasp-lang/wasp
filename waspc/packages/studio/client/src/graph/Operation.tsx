import { Handle, NodeProps, Position } from "reactflow";

export const QueryNode = (props: NodeProps) => (
  <OperationNode
    {...props}
    label={
      <div className="flex items-center">
        <span className="mr-1">Query</span>
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
            d="M9.879 7.519c1.171-1.025 3.071-1.025 4.242 0 1.172 1.025 1.172 2.687 0 3.712-.203.179-.43.326-.67.442-.745.361-1.45.999-1.45 1.827v.75M21 12a9 9 0 11-18 0 9 9 0 0118 0zm-9 5.25h.008v.008H12v-.008z"
          />
        </svg>
      </div>
    }
    color="emerald"
  />
);

export const ActionNode = (props: NodeProps) => (
  <OperationNode
    {...props}
    label={
      <div className="flex items-center">
        <span className="mr-1">Action</span>
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
            d="M11.25 4.5l7.5 7.5-7.5 7.5m-6-15l7.5 7.5-7.5 7.5"
          />
        </svg>
      </div>
    }
    color="pink"
  />
);

export const OperationNode = ({
  data,
  isConnectable,
  targetPosition = Position.Top,
  sourcePosition = Position.Bottom,
  label = "Operation",
  color = "emerald",
}: NodeProps & {
  label?: React.ReactNode;
  color?: string;
}) => (
  <div className={`py-3 px-6 rounded bg-${color}-900 text-white`}>
    <Handle
      type="target"
      position={targetPosition}
      isConnectable={isConnectable}
    />
    <div
      className={`text-xs bg-${color}-300 text-${color}-900 rounded px-1 absolute -top-1 left-1/2 -translate-x-1/2`}
    >
      {label}
    </div>
    <div className="font-bold">{data?.label}</div>
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
  </div>
);
