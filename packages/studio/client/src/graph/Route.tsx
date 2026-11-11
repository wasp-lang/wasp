import { Handle, NodeProps, Position } from "@xyflow/react";
import type { StudioNode } from "./factories";
import { KindIcon, NodeCard } from "./NodeCard";

export const RouteNode = ({
  data,
  isConnectable,
  sourcePosition = Position.Left,
  targetPosition = Position.Right,
}: NodeProps<StudioNode>) => (
  <NodeCard
    kind="Route"
    icon={
      <KindIcon d="M15.042 21.672L13.684 16.6m0 0l-2.51 2.225.569-9.47 5.227 7.917-3.286-.672zM12 2.25V4.5m5.834.166l-1.591 1.591M20.25 10.5H18M7.757 14.743l-1.59 1.59M6 10.5H3.75m4.007-4.243l-1.59-1.59" />
    }
    headerClassName="bg-wasp-yellow-light"
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
  </NodeCard>
);
