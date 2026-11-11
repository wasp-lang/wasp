import { Handle, NodeProps, Position } from "@xyflow/react";
import type { StudioNode } from "./factories";
import { KindIcon, NodeCard } from "./NodeCard";

export const QueryNode = (props: NodeProps<StudioNode>) => (
  <OperationNode
    {...props}
    kind="Query"
    icon={
      <KindIcon d="M9.879 7.519c1.171-1.025 3.071-1.025 4.242 0 1.172 1.025 1.172 2.687 0 3.712-.203.179-.43.326-.67.442-.745.361-1.45.999-1.45 1.827v.75M21 12a9 9 0 11-18 0 9 9 0 0118 0zm-9 5.25h.008v.008H12v-.008z" />
    }
    headerClassName="bg-wasp-purple-light"
  />
);

export const ActionNode = (props: NodeProps<StudioNode>) => (
  <OperationNode
    {...props}
    kind="Action"
    icon={<KindIcon d="M11.25 4.5l7.5 7.5-7.5 7.5m-6-15l7.5 7.5-7.5 7.5" />}
    headerClassName="bg-wasp-purple-quite-light"
  />
);

const OperationNode = ({
  data,
  isConnectable,
  targetPosition = Position.Top,
  sourcePosition = Position.Bottom,
  kind,
  icon,
  headerClassName,
}: NodeProps<StudioNode> & {
  kind: string;
  icon: React.ReactNode;
  headerClassName: string;
}) => (
  <NodeCard
    kind={kind}
    icon={icon}
    headerClassName={headerClassName}
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
