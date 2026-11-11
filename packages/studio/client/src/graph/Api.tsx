import { Handle, NodeProps, Position } from "@xyflow/react";
import type { StudioNode } from "./factories";
import { Chip, KindIcon, NodeCard } from "./NodeCard";

export const ApiNode = ({
  data,
  sourcePosition = Position.Right,
  isConnectable,
}: NodeProps<StudioNode>) => (
  <NodeCard
    kind="API"
    icon={
      <KindIcon d="M14.25 9.75L16.5 12l-2.25 2.25m-4.5 0L7.5 12l2.25-2.25M6 20.25h12A2.25 2.25 0 0020.25 18V6A2.25 2.25 0 0018 3.75H6A2.25 2.25 0 003.75 6v12A2.25 2.25 0 006 20.25z" />
    }
    headerClassName="bg-wasp-g2"
    name={data?.label}
  >
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
    <Chip>
      <strong className="font-bold">{data.httpRoute.method}</strong>{" "}
      {data.httpRoute.path}
    </Chip>
  </NodeCard>
);
