import { Handle, NodeProps, Position } from "reactflow";

export const ApiNode = ({
  data,
  sourcePosition = Position.Right,
  isConnectable,
}: NodeProps) => (
  <div className="py-3 px-6 rounded bg-slate-900 text-white">
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
    <div className="text-xs bg-slate-300 text-slate-900 rounded px-1 absolute -top-1 left-1/2 -translate-x-1/2 flex items-center">
      <span className="mr-1">API</span>
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
          d="M12 6.042A8.967 8.967 0 006 3.75c-1.052 0-2.062.18-3 .512v14.25A8.987 8.987 0 016 18c2.305 0 4.408.867 6 2.292m0-14.25a8.966 8.966 0 016-2.292c1.052 0 2.062.18 3 .512v14.25A8.987 8.987 0 0018 18a8.967 8.967 0 00-6 2.292m0-14.25v14.25"
        />
      </svg>
    </div>
    <div className="font-bold">{data?.label}</div>
    <div className="flex justify-center items-center mt-2">
      <div className="text-xs bg-foreground text-background rounded px-1">
        <span>
          <strong className="font-bold">{data.httpRoute.method}</strong>{" "}
          {data.httpRoute.path}
        </span>
      </div>
    </div>
  </div>
);
