import { Handle, NodeProps, Position } from "reactflow";

export const ApiNode = ({
  data,
  sourcePosition = Position.Right,
  isConnectable,
}: NodeProps) => (
  <div
    className={`
  py-3 px-6 rounded bg-slate-900 text-white text-center
  `}
  >
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
          d="M14.25 9.75L16.5 12l-2.25 2.25m-4.5 0L7.5 12l2.25-2.25M6 20.25h12A2.25 2.25 0 0020.25 18V6A2.25 2.25 0 0018 3.75H6A2.25 2.25 0 003.75 6v12A2.25 2.25 0 006 20.25z"
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
