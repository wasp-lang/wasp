import { useState, useEffect, useRef } from "react";
import SectionContainer from "./Layouts/SectionContainer";

const properties = [
  {
    title: "High Level",
    body: "A uniquely powerful config layer serves as a backbone that connects all parts of the stack (FE, BE, DB, deployment, ...).",
  },
  {
    title: "Batteries Included",
    body: "Like Rails and Laravel, focus on your app, not your framework. Auth, jobs, email, deploy and much more built in.",
  },
  {
    title: "Fully Yours",
    body: "Open source, no lock-in. No provider or cloud platform dependency. Host anywhere.",
  },
];

/* ─────────── Badge styles to cycle through ─────────── */

const badgeBase =
  "inline-block bg-wasp-yellow px-4 py-1.5 font-mono text-xs font-bold uppercase tracking-widest text-wasp-black";

const badgeStyles = [
  {
    name: "Solid border",
    render: (text) => (
      <div className={`${badgeBase} border-2 border-wasp-black`}>{text}</div>
    ),
  },
  {
    name: "Dashed border",
    render: (text) => (
      <div className={`${badgeBase} border-2 border-dashed border-wasp-black`}>
        {text}
      </div>
    ),
  },
  {
    name: "Corner marks",
    render: (text) => (
      <div className="relative inline-block">
        <div className={badgeBase}>{text}</div>
        {/* 4 corner L-marks */}
        <span className="pointer-events-none absolute -left-1 -top-1 h-2 w-2 border-l-2 border-t-2 border-wasp-black" />
        <span className="pointer-events-none absolute -right-1 -top-1 h-2 w-2 border-r-2 border-t-2 border-wasp-black" />
        <span className="pointer-events-none absolute -bottom-1 -left-1 h-2 w-2 border-b-2 border-l-2 border-wasp-black" />
        <span className="pointer-events-none absolute -bottom-1 -right-1 h-2 w-2 border-b-2 border-r-2 border-wasp-black" />
      </div>
    ),
  },
  {
    name: "Corner marks (no fill)",
    render: (text) => (
      <div className="relative inline-block">
        <div className="inline-block px-4 py-1.5 font-mono text-xs font-bold uppercase tracking-widest text-wasp-black">
          {text}
        </div>
        {/* 4 corner L-marks */}
        <span className="pointer-events-none absolute -left-1 -top-1 h-2 w-2 border-l-2 border-t-2 border-wasp-black" />
        <span className="pointer-events-none absolute -right-1 -top-1 h-2 w-2 border-r-2 border-t-2 border-wasp-black" />
        <span className="pointer-events-none absolute -bottom-1 -left-1 h-2 w-2 border-b-2 border-l-2 border-wasp-black" />
        <span className="pointer-events-none absolute -bottom-1 -right-1 h-2 w-2 border-b-2 border-r-2 border-wasp-black" />
      </div>
    ),
  },
  {
    name: "Outline only",
    render: (text) => (
      <div
        className={`inline-block border-2 border-wasp-yellow-dark bg-transparent px-4 py-1.5 font-mono text-xs font-bold uppercase tracking-widest text-wasp-yellow-dark`}
      >
        {text}
      </div>
    ),
  },
  {
    name: "Brackets only",
    render: (text) => (
      <div className="inline-block px-2 py-1.5 font-mono text-xs font-bold uppercase tracking-widest text-wasp-black">
        [ <span className="text-wasp-g5">{text}</span> ]
      </div>
    ),
  },
  {
    name: "Borderless",
    render: (text) => <div className={badgeBase}>{text}</div>,
  },
];

const BadgePicker = ({ text, defaultIdx = 0 }) => {
  const [idx, setIdx] = useState(defaultIdx);
  const [hover, setHover] = useState(false);
  const [menuOpen, setMenuOpen] = useState(false);
  const wrapperRef = useRef(null);

  const cycle = (delta) => setIdx((i) => (i + delta + badgeStyles.length) % badgeStyles.length);

  // Close menu on click outside
  useEffect(() => {
    if (!menuOpen) return;
    const onClick = (e) => {
      if (wrapperRef.current && !wrapperRef.current.contains(e.target)) {
        setMenuOpen(false);
      }
    };
    document.addEventListener("mousedown", onClick);
    return () => document.removeEventListener("mousedown", onClick);
  }, [menuOpen]);

  return (
    <div
      ref={wrapperRef}
      onMouseEnter={() => setHover(true)}
      onMouseLeave={() => setHover(false)}
      className="relative mb-6 inline-block"
    >
      {/* Arrow buttons hug the badge (w-10 = 40px, touching left/right edges)
          so there's no gap that triggers mouseleave when moving toward them. */}
      {(hover || menuOpen) && (
        <>
          <button
            type="button"
            onClick={() => cycle(-1)}
            className="absolute -left-10 top-0 flex h-full w-10 items-center justify-center border-0 bg-transparent p-0 font-mono text-sm text-wasp-g6 hover:text-wasp-black"
            aria-label="Previous badge style"
          >
            ←
          </button>
          <button
            type="button"
            onClick={() => cycle(1)}
            className="absolute -right-10 top-0 flex h-full w-10 items-center justify-center border-0 bg-transparent p-0 font-mono text-sm text-wasp-g6 hover:text-wasp-black"
            aria-label="Next badge style"
          >
            →
          </button>
          <button
            type="button"
            onClick={() => setMenuOpen((o) => !o)}
            className="absolute -top-7 left-1/2 flex h-7 w-10 -translate-x-1/2 items-end justify-center border-0 bg-transparent p-0 pb-0.5 font-mono text-sm text-wasp-g6 hover:text-wasp-black"
            aria-label="Open badge style menu"
          >
            ↑
          </button>
          <div className="absolute -bottom-5 left-0 whitespace-nowrap font-mono text-[10px] uppercase tracking-wider text-wasp-g5">
            {badgeStyles[idx].name}
          </div>
        </>
      )}
      {badgeStyles[idx].render(text)}

      {menuOpen && (
        <div className="absolute left-0 top-full z-50 mt-2 grid w-[640px] grid-cols-3 gap-3 border border-wasp-g3 bg-wasp-bg p-4 shadow-md">
          {badgeStyles.map((s, i) => (
            <button
              key={s.name}
              type="button"
              onClick={() => {
                setIdx(i);
                setMenuOpen(false);
              }}
              className={`flex flex-col items-center justify-between gap-4 border bg-transparent px-3 py-5 text-center font-mono text-[10px] uppercase tracking-wider hover:bg-wasp-g1 ${
                i === idx
                  ? "border-wasp-black text-wasp-black"
                  : "border-wasp-g2 text-wasp-g5"
              }`}
            >
              <div className="flex min-h-[40px] items-center justify-center">
                {s.render(text)}
              </div>
              <span>{s.name}</span>
            </button>
          ))}
        </div>
      )}
    </div>
  );
};

/* ─────────── Variant Box Corners — corner marks at card corners ─────────── */
const VariantBoxCorners = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p) => (
        <div key={p.title} className="relative bg-wasp-bg-2 p-6">
          {/* 4 corner L-marks at box edges */}
          <span className="pointer-events-none absolute -left-1 -top-1 h-3 w-3 border-l-2 border-t-2 border-wasp-black" />
          <span className="pointer-events-none absolute -right-1 -top-1 h-3 w-3 border-r-2 border-t-2 border-wasp-black" />
          <span className="pointer-events-none absolute -bottom-1 -left-1 h-3 w-3 border-b-2 border-l-2 border-wasp-black" />
          <span className="pointer-events-none absolute -bottom-1 -right-1 h-3 w-3 border-b-2 border-r-2 border-wasp-black" />
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="inline-block h-3 w-3 border border-wasp-black bg-wasp-yellow" />
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant Title Corners — corner marks around the title ─────────── */
const VariantTitleCorners = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p) => (
        <div key={p.title} className="border border-wasp-g3 bg-wasp-bg-2 p-6">
          <h3 className="mb-3 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="relative inline-block px-2 py-0.5">
              {p.title}
              {/* 4 corner L-marks around the title */}
              <span className="pointer-events-none absolute -left-1 -top-1 h-2 w-2 border-l-2 border-t-2 border-wasp-black" />
              <span className="pointer-events-none absolute -right-1 -top-1 h-2 w-2 border-r-2 border-t-2 border-wasp-black" />
              <span className="pointer-events-none absolute -bottom-1 -left-1 h-2 w-2 border-b-2 border-l-2 border-wasp-black" />
              <span className="pointer-events-none absolute -bottom-1 -right-1 h-2 w-2 border-b-2 border-r-2 border-wasp-black" />
            </span>
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant Yellow + Paper Numbers — yellow bg, paper-bg numbers, codebox border ─────────── */
const VariantYellowPaperNumbers = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p, i) => (
        <div
          key={p.title}
          className="border border-wasp-g3 bg-wasp-yellow p-6"
        >
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="bg-wasp-black px-1.5 text-wasp-yellow">
              {String(i + 1).padStart(2, "0")}
            </span>
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g7">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant Numbers — 01/02/03 instead of glyphs ─────────── */
const VariantNumbers = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p, i) => (
        <div key={p.title} className="border border-wasp-g3 bg-wasp-bg-2 p-6">
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="bg-wasp-yellow px-1.5 text-wasp-black">
              {String(i + 1).padStart(2, "0")}
            </span>
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant Yellow Border — main yellow border, paper bg ─────────── */
const VariantYellowBorder = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p) => (
        <div
          key={p.title}
          className="border-2 border-wasp-yellow bg-wasp-bg p-6"
        >
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="inline-block h-3 w-3 border border-wasp-black bg-wasp-yellow" />
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant Yellow — full brand-yellow bg ─────────── */
const VariantYellow = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p) => (
        <div
          key={p.title}
          className="border-2 border-wasp-black bg-wasp-yellow p-6"
        >
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="inline-block h-3 w-3 border border-wasp-black bg-wasp-bg" />
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g7">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant Bracket — {} replace the square glyphs ─────────── */
const VariantBracket = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p, i) => (
        <div key={p.title} className="border border-wasp-g3 bg-wasp-bg-2 p-6">
          <h3 className="mb-3 font-mono text-lg font-bold uppercase text-wasp-black">
            <span
              className={`inline-flex items-center gap-2 ${
                i === 1 ? "bg-wasp-yellow-light px-1.5" : ""
              }`}
            >
              <span className={i === 1 ? "text-wasp-black" : "text-wasp-yellow-dark"}>
                {"{}"}
              </span>
              {p.title}
            </span>
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant Asymmetric — 1 big + 2 small ─────────── */
const VariantAsymmetric = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3 lg:grid-rows-2">
      {properties.map((p, i) => (
        <div
          key={p.title}
          className={`border border-wasp-g3 bg-wasp-bg-2 p-6 ${
            i === 0 ? "lg:col-span-2 lg:row-span-2 flex flex-col justify-center" : ""
          }`}
        >
          <h3
            className={`mb-3 flex items-center gap-2 font-mono font-bold uppercase text-wasp-black ${
              i === 0 ? "text-2xl" : "text-lg"
            }`}
          >
            <span
              className={`inline-block border border-wasp-black bg-wasp-yellow ${
                i === 0 ? "h-4 w-4" : "h-3 w-3"
              }`}
            />
            {p.title}
          </h3>
          <p
            className={`font-mono leading-relaxed text-wasp-g6 ${
              i === 0 ? "text-base" : "text-sm"
            }`}
          >
            {p.body}
          </p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant 0 — Martin-ish: thick black border + yellow-light bg ─────────── */
const Variant0 = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p, i) => (
        <div
          key={p.title}
          className="border-2 border-wasp-black bg-wasp-yellow-light p-6"
        >
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="bg-wasp-yellow px-1.5 text-wasp-black">
              {String(i + 1).padStart(2, "0")}
            </span>
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant 1 — Bordered cards (with bg) ─────────── */
const Variant1 = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p) => (
        <div key={p.title} className="border border-wasp-g3 bg-wasp-bg-2 p-6">
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="inline-block h-3 w-3 border border-wasp-black bg-wasp-yellow" />
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

/* ─────────── Variant 2 — Filled cards, no border ─────────── */
const Variant2 = () => (
  <SectionContainer className="pt-4 pb-12">
    <BadgePicker text="// Properties" defaultIdx={2} />
    <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
      {properties.map((p) => (
        <div key={p.title} className="bg-wasp-bg-2 p-6">
          <h3 className="mb-3 flex items-center gap-2 font-mono text-lg font-bold uppercase text-wasp-black">
            <span className="inline-block h-3 w-3 border border-wasp-black bg-wasp-yellow" />
            {p.title}
          </h3>
          <p className="font-mono text-sm leading-relaxed text-wasp-g6">{p.body}</p>
        </div>
      ))}
    </div>
  </SectionContainer>
);

const variants = [
  { num: 1, name: "Box Corners — corner marks at card corners", component: VariantBoxCorners },
  { num: 2, name: "Title Corners — corner marks around the title", component: VariantTitleCorners },
  { num: 3, name: "Yellow + Black Numbers — yellow bg, black pill + yellow numbers, codebox border", component: VariantYellowPaperNumbers },
  { num: 4, name: "Numbers — 01/02/03 instead of glyphs", component: VariantNumbers },
  { num: 5, name: "Yellow Border — main yellow border, paper bg", component: VariantYellowBorder },
  { num: 6, name: "Yellow — full brand-yellow bg", component: VariantYellow },
  { num: 7, name: "Bracket — {} replace the square glyphs", component: VariantBracket },
  { num: 8, name: "Asymmetric — 1 big + 2 small", component: VariantAsymmetric },
  { num: 9, name: "Martin-ish — thick black border + yellow-light bg", component: Variant0 },
  { num: 10, name: "Bordered cards — tan bg + gray border", component: Variant1 },
  { num: 11, name: "Filled cards — tan bg, no border", component: Variant2 },
];

const Properties = () => {
  const [idx, setIdx] = useState(0);
  const cycle = (d) => setIdx((i) => (i + d + variants.length) % variants.length);
  const Active = variants[idx].component;

  return (
    <div>
      <Active />
      <div className="sticky bottom-0 z-40 border-y border-wasp-g3 bg-wasp-bg">
        <div className="relative mx-auto max-w-[1400px] px-6 py-2">
          <button
            type="button"
            onClick={() => cycle(-1)}
            className="absolute left-6 top-1/2 -translate-y-1/2 border-0 bg-transparent p-1 font-mono text-base text-wasp-g6 hover:text-wasp-black"
            aria-label="Previous variant"
          >
            ←
          </button>
          <div className="text-center">
            <span className="font-mono text-xs uppercase tracking-[2px] text-wasp-g7">
              #{variants[idx].num} — {variants[idx].name}
              <span className="ml-3 text-wasp-g5">
                ({idx + 1}/{variants.length})
              </span>
            </span>
          </div>
          <button
            type="button"
            onClick={() => cycle(1)}
            className="absolute right-6 top-1/2 -translate-y-1/2 border-0 bg-transparent p-1 font-mono text-base text-wasp-g6 hover:text-wasp-black"
            aria-label="Next variant"
          >
            →
          </button>
        </div>
      </div>
    </div>
  );
};

export default Properties;
