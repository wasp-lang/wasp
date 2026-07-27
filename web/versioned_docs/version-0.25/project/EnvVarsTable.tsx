import React from "react";
import "./EnvVarsTable.css";

// @ts-ignore
import { Optional, Required } from "@site/src/components/Tag";

type EnvVar = {
  name: string;
  type: "URL" | "string" | "number" | "boolean";
  isRequired?: boolean;
  note: React.ReactNode;
  defaultValue?: string;
};

export function EnvVarsTable({ envVars }: { envVars: EnvVar[] }) {
  return (
    <table className="env-vars-table">
      <thead>
        <tr>
          <th>Name</th>
          <th>Type</th>
          <th>Notes</th>
        </tr>
      </thead>
      <tbody>
        {envVars.map((envVar) => (
          <EnvVarRow key={envVar.name} {...envVar} />
        ))}
      </tbody>
    </table>
  );
}

function EnvVarRow({
  name,
  type,
  isRequired = false,
  note,
  defaultValue,
}: EnvVar) {
  const requiredQualifier = isRequired ? <Required /> : <Optional />;
  return (
    <tr>
      <td>
        <code>{name}</code>
      </td>
      <td>
        <span className="env-var-type">
          <span>{type}</span>
          <span>{requiredQualifier}</span>
        </span>
        {defaultValue && (
          <span className="env-var-default">
            Default: <code>{defaultValue}</code>
          </span>
        )}
      </td>
      <td>{note}</td>
    </tr>
  );
}
