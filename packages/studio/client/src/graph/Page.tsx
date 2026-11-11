import { Handle, NodeProps, Position } from "@xyflow/react";
import type { StudioNode } from "./factories";
import { KindIcon, NodeCard } from "./NodeCard";

export const PageNode = ({
  data,
  isConnectable,
  targetPosition = Position.Right,
}: NodeProps<StudioNode>) => (
  <NodeCard
    kind="Page"
    icon={
      <KindIcon d="M12 6.042A8.967 8.967 0 006 3.75c-1.052 0-2.062.18-3 .512v14.25A8.987 8.987 0 016 18c2.305 0 4.408.867 6 2.292m0-14.25a8.966 8.966 0 016-2.292c1.052 0 2.062.18 3 .512v14.25A8.987 8.987 0 0018 18a8.967 8.967 0 00-6 2.292m0-14.25v14.25" />
    }
    headerClassName="bg-wasp-yellow-quite-light"
    name={
      <>
        {data?.label}
        {data.authRequired ? (
          <span className="ml-1" role="img" aria-label="Auth Required">
            🔒
          </span>
        ) : null}
      </>
    }
  >
    <Handle
      type="target"
      position={targetPosition}
      isConnectable={isConnectable}
    />
  </NodeCard>
);
