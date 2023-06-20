import React, { useMemo } from "react";
export function FileTree({ paths, activeFilePath, onActivePathSelect }) {
  const tree = useMemo(() => {
    if (paths && paths.length > 0) {
      const tree = {};
      paths.forEach((path) => {
        const pathParts = path.split("/");
        let node = tree;
        pathParts.forEach((part, i) => {
          if (i === pathParts.length - 1) {
            node[part] = path;
          } else {
            if (!node[part]) {
              node[part] = {};
            }
            node = node[part];
          }
        });
      });
      return tree;
    } else {
      return null;
    }
  }, [paths]);

  return (
    <div className="flex flex-col gap-2">
      {paths.map((path) => (
        <div
          key={path}
          className={
            "px-4 py-2 bg-slate-200 rounded cursor-pointer " +
            (activeFilePath === path ? "bg-yellow-400" : "")
          }
          onClick={() => onActivePathSelect(path)}
        >
          <div className="font-bold">{path}</div>
        </div>
      ))}
    </div>
  );
}
