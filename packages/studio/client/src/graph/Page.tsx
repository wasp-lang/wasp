import { Handle, NodeProps, Position } from "@xyflow/react";
import type { StudioNode } from "./factories";
import { Icon, NodeCard } from "./NodeCard";

export const PageNode = ({
  data,
  isConnectable,
  targetPosition = Position.Right,
}: NodeProps<StudioNode>) => (
  <NodeCard
    kind="Page"
    icon={
      <Icon d="M12 6.042A8.967 8.967 0 006 3.75c-1.052 0-2.062.18-3 .512v14.25A8.987 8.987 0 016 18c2.305 0 4.408.867 6 2.292m0-14.25a8.966 8.966 0 016-2.292c1.052 0 2.062.18 3 .512v14.25A8.987 8.987 0 0018 18a8.967 8.967 0 00-6 2.292m0-14.25v14.25" />
    }
    headerClassName="bg-wasp-yellow-quite-light"
    name={
      <span className="flex items-center gap-1.5">
        {data?.label}
        {data.authRequired ? (
          <Icon
            d="M16.5 10.5V6.75a4.5 4.5 0 10-9 0v3.75m-.75 11.25h10.5a2.25 2.25 0 002.25-2.25v-6.75a2.25 2.25 0 00-2.25-2.25H6.75a2.25 2.25 0 00-2.25 2.25v6.75a2.25 2.25 0 002.25 2.25z"
            className="text-wasp-g6 h-3.5 w-3.5"
            label="Auth required"
          />
        ) : null}
      </span>
    }
  >
    <Handle
      type="target"
      position={targetPosition}
      isConnectable={isConnectable}
    />
  </NodeCard>
);
