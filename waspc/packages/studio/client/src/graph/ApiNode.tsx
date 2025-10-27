import { Handle, NodeProps, Position } from "reactflow";
import { DeclNodeDataForDecl } from "../node";

export const ApiNode = ({
  data,
  sourcePosition = Position.Right,
  isConnectable,
}: NodeProps<DeclNodeDataForDecl<"Api">>) => {
  console.log(data)
  return (
    <div className={`rounded bg-slate-900 px-6 py-3 text-center text-white`}>
      <Handle
        type="source"
        position={sourcePosition}
        isConnectable={isConnectable}
      />
      <div className="absolute -top-1 left-1/2 flex -translate-x-1/2 items-center rounded bg-slate-300 px-1 text-xs text-slate-900">
        <span className="mr-1">API</span>
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
            d="M14.25 9.75L16.5 12l-2.25 2.25m-4.5 0L7.5 12l2.25-2.25M6 20.25h12A2.25 2.25 0 0020.25 18V6A2.25 2.25 0 0018 3.75H6A2.25 2.25 0 003.75 6v12A2.25 2.25 0 006 20.25z"
          />
        </svg>
      </div>
      <div className="font-bold">{data.name}</div>
      <div className="mt-2 flex items-center justify-center">
        <div className="bg-foreground text-background rounded px-1 text-xs">
          <span>
            <strong className="font-bold">{data.value.httpRoute[0]}</strong>{" "}
            {data.value.httpRoute[1]}
          </span>
        </div>
      </div>
    </div>
  )
}
