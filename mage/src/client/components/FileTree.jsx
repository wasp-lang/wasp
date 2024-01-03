import React, { useMemo } from "react";
import "./FileTree.css";

import TreeView, { flattenTree } from "react-accessible-treeview";
import { DiCss3, DiJavascript, DiNpm, DiReact } from "react-icons/di";
import { FaList, FaRegFolder, FaRegFolderOpen } from "react-icons/fa";
import { WaspIcon } from "./WaspIcon";

export function FileTree({
  paths,
  onActivePathSelect,
  freshlyUpdatedPaths,
}) {
  const tree = useMemo(() => {
    const root = { name: "", children: [] };
    paths.forEach((path) => {
      const pathParts = path.split("/");
      let currentLevel = root;
      pathParts.forEach((part, index) => {
        const existingPath = currentLevel.children.find(
          (child) => child.name === part
        );
        if (existingPath) {
          currentLevel = existingPath;
        } else {
          const metadata = index === pathParts.length - 1 ? { path } : {};
          const newPart = { name: part, children: [], metadata };
          currentLevel.children.push(newPart);
          currentLevel = newPart;
        }
      });
    });
    return root;
  }, [paths]);

  return (
    <DirectoryTreeView
      tree={tree}
      freshlyUpdatedPaths={freshlyUpdatedPaths}
      onNodeSelect={(props) => {
        if (props.element.metadata.path) {
          onActivePathSelect(props.element.metadata.path);
        }
      }}
    />
  );
}

function DirectoryTreeView({ tree, onNodeSelect, freshlyUpdatedPaths }) {
  const data = useMemo(() => {
    return flattenTree(tree);
  }, [tree]);
  const allIds = useMemo(() => {
    return data.map((node) => node.id);
  }, [data]);
  return (
    <div>
      <div className="directory">
        <TreeView
          data={data}
          defaultExpandedIds={allIds}
          expandedIds={allIds}
          aria-label="directory tree"
          onNodeSelect={onNodeSelect}
          nodeRenderer={({
            element,
            isBranch,
            isExpanded,
            getNodeProps,
            level,
          }) => {
            const props = getNodeProps();
            const className = freshlyUpdatedPaths.includes(
              element.metadata.path
            )
              ? `${props.className} freshly-updated-file`
              : props.className;
            props.className = className;

            return (
              <div
                {...props}
                style={{
                  paddingLeft: `calc(0.5rem + ${20 * (level - 1)}px)`,
                }}
              >
                {isBranch ? (
                  <FolderIcon isOpen={isExpanded} />
                ) : (
                  <FileIcon filename={element.name} />
                )}

                {element.name}
              </div>
            );
          }}
        />
      </div>
    </div>
  );
}

const FolderIcon = ({ isOpen }) =>
  isOpen ? (
    <FaRegFolderOpen color="e8a87c" className="icon" />
  ) : (
    <FaRegFolder color="e8a87c" className="icon" />
  );

const FileIcon = ({ filename }) => {
  const extension = filename.slice(filename.lastIndexOf(".") + 1);
  switch (extension) {
    case "js":
    case "ts":
    case "cjs":
      return <DiJavascript color="yellow" className="icon" />;
    case "jsx":
    case "tsx":
      return <DiReact color="turquoise" className="icon" />;
    case "css":
      return <DiCss3 color="turquoise" className="icon" />;
    case "json":
      return <FaList color="yellow" className="icon" />;
    case "npmignore":
      return <DiNpm color="red" className="icon" />;
    case "wasp":
      return <WaspIcon className="icon" />;
    default:
      return null;
  }
};
