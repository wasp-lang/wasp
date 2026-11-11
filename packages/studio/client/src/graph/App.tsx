import { Handle, NodeProps, Position } from "@xyflow/react";
import type { StudioNode } from "./factories";
import { Chip, KindIcon, NodeCard } from "./NodeCard";

export const AppNode = ({
  data,
  isConnectable,
  targetPosition = Position.Left,
  sourcePosition = Position.Right,
}: NodeProps<StudioNode>) => (
  <NodeCard
    kind="App"
    icon={
      <KindIcon d="M8.25 3v1.5M4.5 8.25H3m18 0h-1.5M4.5 12H3m18 0h-1.5m-15 3.75H3m18 0h-1.5M8.25 19.5V21M12 3v1.5m0 15V21m3.75-18v1.5m0 15V21m-9-1.5h10.5a2.25 2.25 0 002.25-2.25V6.75a2.25 2.25 0 00-2.25-2.25H6.75A2.25 2.25 0 004.5 6.75v10.5a2.25 2.25 0 002.25 2.25zm.75-12h9v9h-9v-9z" />
    }
    headerClassName="bg-wasp-yellow"
    name={data?.label}
  >
    <Handle
      type="target"
      position={targetPosition}
      isConnectable={isConnectable}
    />
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
    <Chip>{data.db?.system || "SQLite"}</Chip>
    {data.auth &&
      data.auth.methods.map((method: string) => (
        <Chip key={method}>Auth: {method}</Chip>
      ))}
  </NodeCard>
);
