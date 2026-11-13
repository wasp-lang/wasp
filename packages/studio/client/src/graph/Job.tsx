import { Handle, NodeProps, Position } from "@xyflow/react";
import type { StudioNode } from "./factories";
import { Chip, Icon, NodeCard } from "./NodeCard";

export const JobNode = ({
  data,
  isConnectable,
  sourcePosition = Position.Right,
}: NodeProps<StudioNode>) => (
  <NodeCard
    kind="Job"
    icon={
      <Icon d="M19.5 12c0-1.232-.046-2.453-.138-3.662a4.006 4.006 0 00-3.7-3.7 48.678 48.678 0 00-7.324 0 4.006 4.006 0 00-3.7 3.7c-.017.22-.032.441-.046.662M19.5 12l3-3m-3 3l-3-3m-12 3c0 1.232.046 2.453.138 3.662a4.006 4.006 0 003.7 3.7 48.656 48.656 0 007.324 0 4.006 4.006 0 003.7-3.7c.017-.22.032-.441.046-.662M4.5 12l3 3m-3-3l-3 3" />
    }
    headerClassName="bg-wasp-g1"
    name={data?.label}
  >
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
    {data.schedule && <Chip>Schedule: {data.schedule}</Chip>}
  </NodeCard>
);
