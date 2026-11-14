/// <reference lib="dom" />
import type { ComponentChildren } from "preact";
import { useState } from "preact/hooks";

/** Splits JSON-ish text into tokens for syntax highlighting. */
export function highlightJson(text: string): ComponentChildren[] {
  const tokens: ComponentChildren[] = [];
  const pattern =
    /("(?:\\.|[^"\\])*"(?:\s*:)?)|(-?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)|\b(true|false)\b|\b(null)\b|([{}[\],])/g;
  let last = 0;
  let match: RegExpExecArray | null;
  let key = 0;
  while ((match = pattern.exec(text))) {
    if (match.index > last) tokens.push(text.slice(last, match.index));
    const [token, string, number, boolean, nul] = match;
    const className = string
      ? string.trimEnd().endsWith(":")
        ? "tok-key"
        : "tok-string"
      : number
        ? "tok-number"
        : boolean
          ? "tok-boolean"
          : nul
            ? "tok-null"
            : "tok-punct";
    tokens.push(
      <span key={key++} class={className}>
        {token}
      </span>,
    );
    last = match.index + token.length;
  }
  if (last < text.length) tokens.push(text.slice(last));
  return tokens;
}

export function JsonTree({ value }: { value: unknown }) {
  return (
    <div class="json-tree">
      <JsonNode value={value} depth={0} last />
    </div>
  );
}

function JsonNode({
  name,
  value,
  depth,
  last,
}: {
  name?: string;
  value: unknown;
  depth: number;
  last: boolean;
}) {
  const [open, setOpen] = useState(depth < 3);
  const comma = last ? "" : ",";
  const label =
    name !== undefined ? <span class="tok-key">"{name}": </span> : null;

  if (value !== null && typeof value === "object") {
    const isArray = Array.isArray(value);
    const entries = isArray
      ? (value as unknown[]).map((v, i) => [String(i), v] as const)
      : Object.entries(value as Record<string, unknown>);
    const [openBrace, closeBrace] = isArray ? ["[", "]"] : ["{", "}"];
    if (entries.length === 0) {
      return (
        <div class="json-line">
          {label}
          <span class="tok-punct">
            {openBrace}
            {closeBrace}
          </span>
          {comma}
        </div>
      );
    }
    return (
      <div class="json-node">
        <div class="json-line json-toggle" onClick={() => setOpen(!open)}>
          <span class={`json-caret ${open ? "open" : ""}`}>▸</span>
          {label}
          <span class="tok-punct">{openBrace}</span>
          {!open && (
            <>
              <span class="json-summary">
                {isArray
                  ? `${entries.length} item${entries.length === 1 ? "" : "s"}`
                  : entries
                      .slice(0, 4)
                      .map(([k]) => k)
                      .join(", ") + (entries.length > 4 ? ", …" : "")}
              </span>
              <span class="tok-punct">{closeBrace}</span>
              {comma}
            </>
          )}
        </div>
        {open && (
          <>
            <div class="json-children">
              {entries.map(([k, v], i) => (
                <JsonNode
                  key={k}
                  name={isArray ? undefined : k}
                  value={v}
                  depth={depth + 1}
                  last={i === entries.length - 1}
                />
              ))}
            </div>
            <div class="json-line">
              <span class="tok-punct">{closeBrace}</span>
              {comma}
            </div>
          </>
        )}
      </div>
    );
  }

  return (
    <div class="json-line">
      {label}
      <Primitive value={value} />
      {comma}
    </div>
  );
}

function Primitive({ value }: { value: unknown }) {
  if (value === null) return <span class="tok-null">null</span>;
  if (typeof value === "string") {
    const isDate = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}/.test(value);
    return (
      <span
        class="tok-string"
        title={isDate ? new Date(value).toString() : undefined}
      >
        "{value}"
      </span>
    );
  }
  if (typeof value === "number") return <span class="tok-number">{value}</span>;
  if (typeof value === "boolean")
    return <span class="tok-boolean">{String(value)}</span>;
  return <span>{String(value)}</span>;
}

/** A textarea with JSON syntax highlighting drawn underneath it. */
export function JsonEditor({
  value,
  onInput,
  placeholder,
}: {
  value: string;
  onInput: (value: string) => void;
  placeholder?: string;
}) {
  const lines = value.split("\n").length;
  return (
    <div class="code-editor">
      <div class="code-gutter" aria-hidden="true">
        {Array.from({ length: Math.max(lines, 1) }, (_, i) => (
          <div key={i}>{i + 1}</div>
        ))}
      </div>
      <div class="code-area">
        <pre class="code-highlight" aria-hidden="true">
          {highlightJson(value)}
          {"\n"}
        </pre>
        <textarea
          class="code-input"
          spellcheck={false}
          value={value}
          placeholder={placeholder}
          onInput={(event) => onInput(event.currentTarget.value)}
          onKeyDown={(event) => {
            if (event.key === "Tab") {
              event.preventDefault();
              const target = event.currentTarget;
              const { selectionStart, selectionEnd } = target;
              const next =
                value.slice(0, selectionStart) +
                "  " +
                value.slice(selectionEnd);
              onInput(next);
              requestAnimationFrame(() => {
                target.selectionStart = target.selectionEnd =
                  selectionStart + 2;
              });
            }
          }}
        />
      </div>
    </div>
  );
}
