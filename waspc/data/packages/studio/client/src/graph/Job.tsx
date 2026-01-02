import { Handle, NodeProps, Position } from "reactflow";

export const JobNode = ({
  data,
  isConnectable,
  sourcePosition = Position.Right,
}: NodeProps) => (
  <div className="rounded bg-violet-900 px-6 py-3 text-center text-white">
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
    <div className="absolute -top-1 left-1/2 flex -translate-x-1/2 items-center rounded bg-violet-300 px-1 text-xs text-violet-900">
      <span className="mr-1">Job</span>
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
          d="M19.5 12c0-1.232-.046-2.453-.138-3.662a4.006 4.006 0 00-3.7-3.7 48.678 48.678 0 00-7.324 0 4.006 4.006 0 00-3.7 3.7c-.017.22-.032.441-.046.662M19.5 12l3-3m-3 3l-3-3m-12 3c0 1.232.046 2.453.138 3.662a4.006 4.006 0 003.7 3.7 48.656 48.656 0 007.324 0 4.006 4.006 0 003.7-3.7c.017-.22.032-.441.046-.662M4.5 12l3 3m-3-3l-3 3"
        />
      </svg>
    </div>
    <div className="font-bold">{data?.label}</div>
    {data.schedule && (
      <div className="mt-2 flex items-center justify-center">
        <div className="rounded bg-foreground px-1 text-xs text-background">
          <span>Schedule: {data.schedule}</span>
        </div>
      </div>
    )}
  </div>
);
