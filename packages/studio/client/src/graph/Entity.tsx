import { Handle, NodeProps, Position } from "@xyflow/react";
import type { StudioNode } from "./factories";
import { KindIcon, NodeCard } from "./NodeCard";

export const EntityNode = ({
  data,
  isConnectable,
  targetPosition = Position.Left,
  sourcePosition = Position.Right,
}: NodeProps<StudioNode>) => (
  <NodeCard
    kind={data.isUserEntity ? "Entity · User" : "Entity"}
    icon={
      <KindIcon d="M15.75 6a3.75 3.75 0 11-7.5 0 3.75 3.75 0 017.5 0zM4.501 20.118a7.5 7.5 0 0114.998 0A17.933 17.933 0 0112 21.75c-2.676 0-5.216-.584-7.499-1.632z" />
    }
    headerClassName="bg-wasp-black text-wasp-yellow"
    name={data?.label}
  >
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
    <Handle
      type="target"
      position={targetPosition}
      isConnectable={isConnectable}
    />
  </NodeCard>
);
