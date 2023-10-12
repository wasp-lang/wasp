import { Handle, NodeProps, Position } from "reactflow";

export const JobNode = ({
  data,
  isConnectable,
  sourcePosition = Position.Right,
}: NodeProps) => (
  <div className="py-3 px-6 rounded bg-violet-900 text-white text-center">
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
    <div className="text-xs bg-violet-300 text-violet-900 rounded px-1 absolute -top-1 left-1/2 -translate-x-1/2 flex items-center">
      <span className="mr-1">Job</span>
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
          d="M19.5 12c0-1.232-.046-2.453-.138-3.662a4.006 4.006 0 00-3.7-3.7 48.678 48.678 0 00-7.324 0 4.006 4.006 0 00-3.7 3.7c-.017.22-.032.441-.046.662M19.5 12l3-3m-3 3l-3-3m-12 3c0 1.232.046 2.453.138 3.662a4.006 4.006 0 003.7 3.7 48.656 48.656 0 007.324 0 4.006 4.006 0 003.7-3.7c.017-.22.032-.441.046-.662M4.5 12l3 3m-3-3l-3 3"
        />
      </svg>
    </div>
    <div className="font-bold">{data?.label}</div>
    {data.schedule && (
      <div className="flex justify-center items-center mt-2">
        <div className="text-xs bg-foreground text-background rounded px-1">
          <span>Schedule: {data.schedule}</span>
        </div>
      </div>
    )}
  </div>
);
