import {
  useState,
  useCallback,
  useEffect,
  useLayoutEffect,
  useMemo,
} from "react";
import {
  Navbar,
  NavbarBrand,
  NavbarContent,
  NavbarItem,
  Link,
  Button,
  Modal,
  ModalContent,
  ModalHeader,
  ModalBody,
  ModalFooter,
  useDisclosure,
} from "@nextui-org/react";
import ReactFlow, {
  Controls,
  Background,
  type Edge,
  type Node,
  useReactFlow,
  Position,
  useNodesState,
  useEdgesState,
  Handle,
  NodeProps,
} from "reactflow";
import "reactflow/dist/style.css";

import ELK from "elkjs/lib/elk.bundled.js";

import logo from "./assets/logo.png";

import { socket } from "./socket";

const elk = new ELK();

// Elk has a *huge* amount of options to configure. To see everything you can
// tweak check out:
//
// - https://www.eclipse.org/elk/reference/algorithms.html
// - https://www.eclipse.org/elk/reference/options.html
const elkOptions = {
  "elk.algorithm": "layered",
  "elk.layered.spacing.nodeNodeBetweenLayers": "100",
  "elk.spacing.nodeNode": "40",
};

const getLayoutedElements = (
  // @ts-ignore
  nodes,
  // @ts-ignore
  edges,
  options = {}
) => {
  const graph = {
    id: "root",
    layoutOptions: options,
    children: nodes.map((node: Node) => ({
      ...node,

      // Hardcode a width and height for elk to use when layouting.
      width: 150,
      height: 50,
    })),
    edges: edges,
  };

  return elk
    .layout(graph)
    .then((layoutedGraph) => ({
      // @ts-ignore
      nodes: layoutedGraph.children.map((node) => ({
        ...node,
        // React Flow expects a position property on the node instead of `x`
        // and `y` fields.
        position: { x: node.x, y: node.y },
      })),

      edges: layoutedGraph.edges,
    }))
    .catch(console.error);
};

type Data = {
  pages: {
    id: string;
    name: string;
    flowsInto: string[];
  }[];
  queries: {
    id: string;
    name: string;
    flowsInto: string[];
  }[];
  actions: {
    id: string;
    name: string;
    flowsInto: string[];
  }[];
  entities: {
    id: string;
    name: string;
  }[];
};

export default function App() {
  const [data, setData] = useState<Data | null>(null);

  function onData(data: string) {
    console.log("data", data);
    setData(JSON.parse(data) as Data);
  }

  useEffect(() => {
    socket.on("data", onData);
    return () => {
      socket.off("data", onData);
    };
  }, []);

  return (
    <div className="h-full">
      <Navbar position="static">
        <NavbarBrand>
          <img src={logo} alt="logo" className="w-8 h-8" />
          <p className="font-bold text-inherit ml-4">App Name</p>
        </NavbarBrand>
        {/* <NavbarContent className="hidden sm:flex gap-4" justify="center">
          <NavbarItem>
            <Link color="foreground" href="#">
              Features
            </Link>
          </NavbarItem>
          <NavbarItem isActive>
            <Link href="#" aria-current="page">
              Customers
            </Link>
          </NavbarItem>
          <NavbarItem>
            <Link color="foreground" href="#">
              Integrations
            </Link>
          </NavbarItem>
        </NavbarContent> */}
        <NavbarContent justify="end">
          {/* <NavbarItem className="hidden lg:flex">
            <Link href="#">Login</Link>
          </NavbarItem> */}
          <NavbarItem>
            <Button as={Link} color="primary" href="#" variant="flat">
              Close the Studio
            </Button>
          </NavbarItem>
        </NavbarContent>
      </Navbar>
      <div className="flow-container">
        {data ? (
          <Flow data={data} />
        ) : (
          <div className="flex items-center justify-center h-full">
            <p className="text-2xl text-gray-500">Loading...</p>
          </div>
        )}
      </div>
    </div>
  );
}

const PageNode = ({
  data,
  isConnectable,
  sourcePosition = Position.Bottom,
}: NodeProps) => (
  <div className="py-3 px-6 rounded bg-sky-900 text-white">
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
    <div className="text-xs bg-sky-300 text-sky-900 rounded px-1 absolute -top-1 left-1/2 -translate-x-1/2 flex items-center">
      <span className="mr-1">Page</span>
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
          d="M12 6.042A8.967 8.967 0 006 3.75c-1.052 0-2.062.18-3 .512v14.25A8.987 8.987 0 016 18c2.305 0 4.408.867 6 2.292m0-14.25a8.966 8.966 0 016-2.292c1.052 0 2.062.18 3 .512v14.25A8.987 8.987 0 0018 18a8.967 8.967 0 00-6 2.292m0-14.25v14.25"
        />
      </svg>
    </div>
    <div className="font-bold">{data?.label}</div>
  </div>
);

const QueryNode = (props: NodeProps) => (
  <OperationNode
    {...props}
    label={
      <div className="flex items-center">
        <span className="mr-1">Query</span>
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
            d="M9.879 7.519c1.171-1.025 3.071-1.025 4.242 0 1.172 1.025 1.172 2.687 0 3.712-.203.179-.43.326-.67.442-.745.361-1.45.999-1.45 1.827v.75M21 12a9 9 0 11-18 0 9 9 0 0118 0zm-9 5.25h.008v.008H12v-.008z"
          />
        </svg>
      </div>
    }
    color="emerald"
  />
);

const ActionNode = (props: NodeProps) => (
  <OperationNode
    {...props}
    label={
      <div className="flex items-center">
        <span className="mr-1">Action</span>
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
            d="M11.25 4.5l7.5 7.5-7.5 7.5m-6-15l7.5 7.5-7.5 7.5"
          />
        </svg>
      </div>
    }
    color="pink"
  />
);

const OperationNode = ({
  data,
  isConnectable,
  targetPosition = Position.Top,
  sourcePosition = Position.Bottom,
  label = "Operation",
  color = "emerald",
}: NodeProps & {
  label?: React.ReactNode;
  color?: string;
}) => (
  <div className={`py-3 px-6 rounded bg-${color}-900 text-white`}>
    <Handle
      type="target"
      position={targetPosition}
      isConnectable={isConnectable}
    />
    <div
      className={`text-xs bg-${color}-300 text-${color}-900 rounded px-1 absolute -top-1 left-1/2 -translate-x-1/2`}
    >
      {label}
    </div>
    <div className="font-bold">{data?.label}</div>
    <Handle
      type="source"
      position={sourcePosition}
      isConnectable={isConnectable}
    />
  </div>
);

const EntityNode = ({
  data,
  isConnectable,
  targetPosition = Position.Top,
}: NodeProps) => (
  // <div className="py-3 px-6 rounded bg-yellow-300 text-xs border-1 border-yellow-900">
  //   <Handle
  //     type="target"
  //     position={targetPosition}
  //     isConnectable={isConnectable}
  //   />
  //   {data?.label}
  // </div>
  <div className="py-3 px-6 rounded bg-yellow-900 text-white">
    <Handle
      type="target"
      position={targetPosition}
      isConnectable={isConnectable}
    />
    <div className="text-xs bg-yellow-300 text-yellow-900 rounded px-1 absolute -top-1 left-1/2 -translate-x-1/2 flex items-center">
      <span className="mr-1">Entity</span>
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
          d="M15.75 6a3.75 3.75 0 11-7.5 0 3.75 3.75 0 017.5 0zM4.501 20.118a7.5 7.5 0 0114.998 0A17.933 17.933 0 0112 21.75c-2.676 0-5.216-.584-7.499-1.632z"
        />
      </svg>
    </div>
    <div className="font-bold">{data?.label}</div>
  </div>
);

let id = 1;

// const appNode = createAppNode("App");

// const pagesNode = createGroupNode();

function Flow({ data }: { data: Data }) {
  const initialNodes: Node[] = [
    ...data.pages.map((page) => createPageNode(page.id, page.name)),
    ...data.queries.map((query) => createQueryNode(query.id, query.name)),
    ...data.actions.map((action) => createActionNode(action.id, action.name)),
    ...data.entities.map((entity) => createEntityNode(entity.id, entity.name)),
  ];

  const initialEdges: Edge[] = [
    ...data.pages.flatMap((page) =>
      page.flowsInto.map((flow) => createEdge(page.id, flow))
    ),
    ...data.queries.flatMap((query) =>
      query.flowsInto.map((flow) => createEdge(query.id, flow))
    ),
    ...data.actions.flatMap((action) =>
      action.flowsInto.map((flow) => createEdge(action.id, flow))
    ),
  ];

  console.log("initialNodes", initialNodes);
  console.log("initialEdges", initialEdges);

  const { isOpen, onOpen, onClose } = useDisclosure();
  const [selectedNode, setSelectedNode] = useState<Node | null>(null);

  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const { fitView } = useReactFlow();
  const nodeTypes = useMemo(
    () => ({
      pageNode: PageNode,
      // operationNode: OperationNode,
      entityNode: EntityNode,
      queryNode: QueryNode,
      actionNode: ActionNode,
    }),
    []
  );

  const onLayout = useCallback(() => {
    const opts = { "elk.direction": "RIGHT", ...elkOptions };

    getLayoutedElements(initialNodes, initialEdges, opts).then(
      ({ nodes: layoutedNodes, edges: layoutedEdges }) => {
        setNodes(layoutedNodes);
        setEdges(layoutedEdges);

        window.requestAnimationFrame(() => fitView());
      }
    );
  }, [nodes, edges]);

  // Calculate the initial layout on mount.
  useLayoutEffect(() => {
    onLayout();
  }, []);

  // const onNodesChange: OnNodesChange = useCallback(
  //   (changes) => setNodes((nds) => applyNodeChanges(changes, nds)),
  //   []
  // );
  // const onEdgesChange: OnEdgesChange = useCallback(
  //   (changes) => setEdges((eds) => applyEdgeChanges(changes, eds)),
  //   []
  // );

  useEffect(() => {
    fitView();
  }, [fitView]);

  return (
    <div style={{ height: "100%" }}>
      <ReactFlow
        nodes={nodes}
        edges={edges}
        fitView
        nodeTypes={nodeTypes}
        onNodeClick={(_event, node) => {
          setSelectedNode(node);
          onOpen();
        }}
      >
        <Background
          style={{
            backgroundColor: `hsl(var(--nextui-background)`,
          }}
          color={`#444`}
        />
        {/* <Controls /> */}
      </ReactFlow>

      <Modal size="lg" isOpen={isOpen} onClose={onClose}>
        <ModalContent>
          {(onClose) => (
            <>
              <ModalHeader className="flex flex-col gap-1">
                {selectedNode?.data?.label}
              </ModalHeader>
              <ModalBody>
                <pre>
                  {JSON.stringify(selectedNode?.data, null, 2) || "No data"}
                </pre>
              </ModalBody>
              <ModalFooter>
                {/* <Button color="danger" variant="light" onPress={onClose}>
                  Close
                </Button>
                <Button color="primary" onPress={onClose}>
                  Action
                </Button> */}
              </ModalFooter>
            </>
          )}
        </ModalContent>
      </Modal>
    </div>
  );
}

function createEdge(source: string, target: string, label?: string) {
  return {
    id: `${source}-${target}`,
    source,
    target,
    animated: true,
    // type: "step",
    label,
  } satisfies Edge;
}

function createPageNode(id: string, name: string, parentId?: string) {
  return {
    id,
    type: "pageNode",
    data: { label: name },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    // targetPosition: Position.Left,
    // type: "input",
    parentNode: parentId,
  } satisfies Node;
}

function createActionNode(id: string, name: string, parentId?: string) {
  return {
    id,
    data: { label: name },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    targetPosition: Position.Left,
    parentNode: parentId,
    type: "actionNode",
  } satisfies Node;
}

function createQueryNode(id: string, name: string, parentId?: string) {
  return {
    id,
    data: { label: name },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    targetPosition: Position.Left,
    parentNode: parentId,
    type: "queryNode",
  } satisfies Node;
}

function createEntityNode(id: string, name: string, parentId?: string) {
  return {
    id,
    data: { label: name },
    position: { x: 0, y: 0 },
    type: "entityNode",
    targetPosition: Position.Left,
    // style: {
    //   backgroundColor: `#fde047`,
    // },
    parentNode: parentId,
  } satisfies Node;
}

function createAppNode(name: string, parentId?: string) {
  return {
    id: `app-${id++}`,
    data: { label: name },
    position: { x: 0, y: 0 },
    sourcePosition: Position.Right,
    type: "input",
    parentNode: parentId,
  } satisfies Node;
}

function createGroupNode() {
  return {
    id: `group-${id++}`,
  };
}
