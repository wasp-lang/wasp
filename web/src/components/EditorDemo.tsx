import { useEffect, useRef, useState } from "react";

import CodeHighlight from "./CodeHighlight";

// ---------------------------------------------------------------------------
// File contents the demo "types" out.
// ---------------------------------------------------------------------------

const WASP_BASE = `app myApp {
  wasp: { version: "^0.18.0" },
  title: "myApp",
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import { MainPage } from "@src/MainPage",
}`;

// Same file, with an auth block inserted into the app declaration.
const WASP_AUTH = `app myApp {
  wasp: { version: "^0.18.0" },
  title: "myApp",
  auth: {
    userEntity: User,
    methods: { email: {} },
    onAuthFailedRedirectTo: "/login",
  },
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
  component: import { MainPage } from "@src/MainPage",
}`;

// Same file again, now with a query declaration appended.
const WASP_QUERY = `${WASP_AUTH}

query getTasks {
  fn: import { getTasks } from "@src/queries",
}`;

// The query's implementation, typed into a fresh src/queries.ts file.
const QUERIES_CODE = `import { type GetTasks } from "wasp/server/operations"

export const getTasks: GetTasks<void, string[]> = async () => {
  return ["Buy milk", "Ship my app"]
}`;

const TSX_BASE = `export const MainPage = () => {
  return (
    <main className="home">
      <h1>Welcome to myApp</h1>
    </main>
  )
}`;

// Same component, with one extra line of copy added under the heading.
const TSX_TEXT = `export const MainPage = () => {
  return (
    <main className="home">
      <h1>Welcome to myApp</h1>
      <p>Your full-stack app, ready to ship.</p>
    </main>
  )
}`;

// Marker we splice into the code string to show where the caret sits; it's
// swapped for a (blinking) caret glyph at render time.
const CARET = "";

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type FileName = "main.wasp" | "queries.ts" | "MainPage.tsx" | null;
type Focus = "wasp" | "queries" | "tsx" | "term" | null;
type TermLine = { kind: "cmd" | "out" | "ok"; text: string };

const TreeRow = ({
  label,
  depth,
  active = false,
  visible,
}: {
  label: string;
  depth: number;
  active?: boolean;
  visible: boolean;
}) => (
  <div
    className={`truncate px-2 py-[3px] transition-opacity duration-300 ${
      visible ? "opacity-100" : "opacity-0"
    } ${
      active
        ? "bg-wasp-yellow-light font-semibold text-wasp-black"
        : "text-wasp-g6"
    }`}
    style={{ paddingLeft: `${10 + depth * 16}px` }}
  >
    {label}
  </div>
);

const Dots = ({ dark = false }: { dark?: boolean }) => (
  <div className="flex gap-1.5">
    <span className={`h-3 w-3 rounded-full ${dark ? "bg-wasp-g5" : "bg-wasp-g3"}`} />
    <span className={`h-3 w-3 rounded-full ${dark ? "bg-wasp-g5" : "bg-wasp-g3"}`} />
    <span className={`h-3 w-3 rounded-full ${dark ? "bg-wasp-g5" : "bg-wasp-g3"}`} />
  </div>
);

// A brief yellow flash drawn over a region right after it's populated.
const FlashOverlay = () => (
  <div
    className="pointer-events-none absolute inset-0 z-10 bg-wasp-yellow"
    style={{ animation: "editorDemoFlash 0.85s ease-out forwards" }}
  />
);

const BrowserPane = () => (
  <div className="flex h-full flex-col bg-wasp-white">
    <div className="flex items-center gap-2 border-b border-wasp-g2 px-3 py-2">
      <Dots />
      <div className="flex-1 rounded bg-wasp-g1 px-3 py-1 text-center font-mono text-xs text-wasp-g6">
        localhost:8000
      </div>
    </div>
    <div className="flex flex-1 flex-col items-center justify-center gap-4 px-6 text-center">
      <div className="text-4xl">🐝</div>
      <h1 className="text-2xl font-extrabold uppercase tracking-tight text-wasp-black">
        Welcome to <span className="bg-wasp-yellow px-1">myApp</span>
      </h1>
      <p className="text-sm text-wasp-g6">Your full-stack app, ready to ship.</p>
      <button
        type="button"
        className="cursor-default border-2 border-wasp-black bg-wasp-yellow px-4 py-2 text-sm font-semibold text-wasp-black"
      >
        Log in
      </button>
    </div>
  </div>
);

// ---------------------------------------------------------------------------
// Main component
// ---------------------------------------------------------------------------

const EditorDemo = () => {
  const [tree, setTree] = useState(false);
  const [active, setActive] = useState<FileName>(null);
  const [waspCode, setWaspCode] = useState("");
  const [queriesCode, setQueriesCode] = useState("");
  const [tsxCode, setTsxCode] = useState("");
  const [termLines, setTermLines] = useState<TermLine[]>([]);
  const [termInput, setTermInput] = useState("");
  const [focus, setFocus] = useState<Focus>(null);
  const [browser, setBrowser] = useState(false);
  const [caretOn, setCaretOn] = useState(true);
  const [flashTree, setFlashTree] = useState(0);
  const [flashMain, setFlashMain] = useState(0);

  const termRef = useRef<HTMLDivElement>(null);

  // Keep the terminal scrolled to the latest line.
  useEffect(() => {
    if (termRef.current) termRef.current.scrollTop = termRef.current.scrollHeight;
  }, [termLines, termInput]);

  // Blinking caret.
  useEffect(() => {
    const id = setInterval(() => setCaretOn((on) => !on), 530);
    return () => clearInterval(id);
  }, []);

  useEffect(() => {
    let cancelled = false;
    const sleep = (ms: number) =>
      new Promise<void>((resolve) => setTimeout(resolve, ms));

    const typeInput = async (text: string, perChar: number) => {
      for (let i = 1; i <= text.length; i++) {
        if (cancelled) return;
        setTermInput(text.slice(0, i));
        await sleep(perChar);
      }
    };

    // Open a file at the insertion point (caret lands), pause, then type the
    // inserted chunk character by character with the caret trailing it.
    const placeCaretThenType = async (
      setter: (s: string) => void,
      from: string,
      to: string,
      perChar: number,
      landMs: number,
    ) => {
      let p = 0;
      while (p < from.length && from[p] === to[p]) p++;
      let s = 0;
      while (
        s < from.length - p &&
        from[from.length - 1 - s] === to[to.length - 1 - s]
      )
        s++;
      const prefix = to.slice(0, p);
      const middle = to.slice(p, to.length - s);
      const suffix = to.slice(to.length - s);

      setter(prefix + CARET + suffix); // file open, caret positioned
      await sleep(landMs);
      for (let i = 1; i <= middle.length; i++) {
        if (cancelled) return;
        setter(prefix + middle.slice(0, i) + CARET + suffix);
        await sleep(perChar);
      }
    };

    const pushLine = (line: TermLine) =>
      setTermLines((prev) => [...prev, line]);

    const run = async () => {
      while (!cancelled) {
        // ----- reset -----
        setTree(false);
        setActive(null);
        setWaspCode("");
        setQueriesCode("");
        setTsxCode("");
        setTermLines([]);
        setTermInput("");
        setBrowser(false);
        setFocus("term");
        await sleep(1300);
        if (cancelled) return;

        // ----- 1. create the project -----
        await typeInput("wasp new myApp", 75);
        await sleep(700);
        if (cancelled) return;
        pushLine({ kind: "cmd", text: "wasp new myApp" });
        setTermInput("");
        await sleep(800);
        pushLine({ kind: "ok", text: "🐝 Created a new Wasp app in ./myApp" });
        await sleep(900);
        if (cancelled) return;

        // ----- 2. files appear, open main.wasp -----
        setTree(true);
        setFlashTree((n) => n + 1);
        await sleep(1100);
        if (cancelled) return;
        setActive("main.wasp");
        setFocus("wasp");

        // ----- 3. add authentication -----
        await placeCaretThenType(setWaspCode, WASP_BASE, WASP_AUTH, 42, 900);
        if (cancelled) return;
        setFlashMain((n) => n + 1);
        await sleep(1400);
        if (cancelled) return;

        // ----- 4. declare a query -----
        await placeCaretThenType(setWaspCode, WASP_AUTH, WASP_QUERY, 42, 700);
        if (cancelled) return;
        setFlashMain((n) => n + 1);
        await sleep(1400);
        if (cancelled) return;

        // ----- 5. implement the query in src/queries.ts -----
        setActive("queries.ts");
        setFocus("queries");
        await placeCaretThenType(setQueriesCode, "", QUERIES_CODE, 38, 800);
        if (cancelled) return;
        setFlashMain((n) => n + 1);
        await sleep(1400);
        if (cancelled) return;

        // ----- 6. edit the React component -----
        setActive("MainPage.tsx");
        setFocus("tsx");
        await placeCaretThenType(setTsxCode, TSX_BASE, TSX_TEXT, 46, 850);
        if (cancelled) return;
        setFlashMain((n) => n + 1);
        await sleep(1400);
        if (cancelled) return;

        // ----- 7. run the app -----
        setFocus("term");
        await typeInput("wasp start", 75);
        await sleep(650);
        if (cancelled) return;
        pushLine({ kind: "cmd", text: "wasp start" });
        setTermInput("");
        await sleep(800);
        pushLine({ kind: "out", text: "🚀 Starting your app..." });
        await sleep(750);
        pushLine({ kind: "ok", text: "✓ Database  running on :5432" });
        await sleep(650);
        pushLine({ kind: "ok", text: "✓ Server    running on :3001" });
        await sleep(650);
        pushLine({ kind: "ok", text: "✓ Client    running on :8000" });
        await sleep(900);
        if (cancelled) return;

        // ----- 8. the app loads in the browser -----
        setFocus(null);
        setBrowser(true);
        setFlashMain((n) => n + 1);
        await sleep(4000);
        if (cancelled) return;

        // ----- 9. ship it -----
        setFocus("term");
        await typeInput("wasp deploy railway", 75);
        await sleep(650);
        if (cancelled) return;
        pushLine({ kind: "cmd", text: "wasp deploy railway" });
        setTermInput("");
        await sleep(800);
        pushLine({ kind: "out", text: "📦 Building for production..." });
        await sleep(900);
        pushLine({ kind: "ok", text: "✓ Live at https://myapp.up.railway.app" });
        await sleep(900);
        setFocus(null);
        await sleep(4500);
      }
    };

    run();
    return () => {
      cancelled = true;
    };
  }, []);

  const currentFile =
    active === "main.wasp"
      ? { code: waspCode, language: "wasp" }
      : active === "queries.ts"
        ? { code: queriesCode, language: "typescript" }
        : active === "MainPage.tsx"
          ? { code: tsxCode, language: "tsx" }
          : null;
  // Swap the caret marker for a blinking glyph (or a same-width space).
  const renderCode = (code: string) =>
    code.replace(CARET, caretOn ? "│" : " ");

  return (
    <div className="flex h-[500px] w-full flex-col border-2 border-wasp-black bg-[#f0ede6] font-mono text-sm sm:h-[580px]">
      <style>{`@keyframes editorDemoFlash{from{opacity:.5}to{opacity:0}}`}</style>

      {/* Title bar */}
      <div className="flex items-center gap-3 border-b-2 border-wasp-black bg-wasp-g7 px-4 py-2.5">
        <Dots dark />
        <span className="ml-1 text-xs text-wasp-g3">myApp — Wasp</span>
      </div>

      {/* Editor: file tree + main pane */}
      <div className="flex min-h-0 flex-1">
        {/* File tree */}
        <div className="relative w-[148px] shrink-0 overflow-hidden border-r border-wasp-g3 bg-wasp-bg py-2 text-xs">
          {flashTree > 0 && <FlashOverlay key={flashTree} />}
          <TreeRow
            label="main.wasp"
            depth={0}
            visible={tree}
            active={active === "main.wasp"}
          />
          <TreeRow label="src/" depth={0} visible={tree} />
          <TreeRow
            label="MainPage.tsx"
            depth={1}
            visible={tree}
            active={active === "MainPage.tsx"}
          />
          <TreeRow
            label="queries.ts"
            depth={1}
            visible={tree}
            active={active === "queries.ts"}
          />
          <TreeRow label="Main.css" depth={1} visible={tree} />
          <TreeRow label="schema.prisma" depth={0} visible={tree} />
          <TreeRow label="package.json" depth={0} visible={tree} />
        </div>

        {/* Main pane: code editor or browser preview */}
        <div
          className={`relative min-w-0 flex-1 overflow-auto ${
            !browser && (focus === "wasp" || focus === "queries" || focus === "tsx")
              ? "ring-1 ring-inset ring-wasp-yellow"
              : ""
          }`}
        >
          {flashMain > 0 && <FlashOverlay key={flashMain} />}
          {browser ? (
            <BrowserPane />
          ) : currentFile && currentFile.code ? (
            <CodeHighlight
              language={currentFile.language}
              source={renderCode(currentFile.code)}
            />
          ) : (
            <div className="flex h-full items-center justify-center text-xs text-wasp-g4">
              No file open
            </div>
          )}
        </div>
      </div>

      {/* Terminal */}
      <div
        ref={termRef}
        className="h-[200px] shrink-0 overflow-auto border-t-2 border-wasp-black bg-wasp-black px-4 py-3 text-[13px] leading-relaxed text-[#d4d4d4]"
      >
        {termLines.map((line, i) => (
          <div key={i} className="whitespace-pre-wrap">
            {line.kind === "cmd" ? (
              <>
                <span className="text-wasp-yellow">$</span> {line.text}
              </>
            ) : line.kind === "ok" ? (
              <span className="text-[#27c93f]">{line.text}</span>
            ) : (
              <span className="text-[#9aa0a6]">{line.text}</span>
            )}
          </div>
        ))}
        {/* Live prompt */}
        <div className="whitespace-pre-wrap">
          <span className="text-wasp-yellow">$</span> {termInput}
          {focus === "term" && (
            <span className={caretOn ? "text-wasp-yellow" : "text-transparent"}>
              │
            </span>
          )}
        </div>
      </div>
    </div>
  );
};

export default EditorDemo;
