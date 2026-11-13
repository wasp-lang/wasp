import useBaseUrl from "@docusaurus/useBaseUrl";

import styles from "./HowItWorksDiagram.module.css";

/* Compile pipeline diagram for the "How it works" section.

   Layout discipline:
   - Arrow rail at y=140 — every stage (boxes, arrows, globe) centers on it.
   - The YOUR CODE box and the WASP CLI frame share the same extent (y=38-242).
   - COMPILE and DEPLOY are centered on the frame's left/right edges, so each
     arrow sits half inside the frame and half outside; a white plate keeps the
     label legible where it crosses the striped fill.
   - File rows in "your code" all share the same indent — flat hierarchy.

   Colours live in HowItWorksDiagram.module.css, with a light and a dark
   variant each. The only raw hex left here is the React/Node brand colours. */
const HowItWorksDiagram = ({ className = "" }: { className?: string }) => (
  <svg
    viewBox="0 0 1000 260"
    preserveAspectRatio="xMidYMid meet"
    aria-label="Wasp compiles your code into an intermediate representation, then generates a full-stack app (frontend, backend, database) ready to deploy anywhere."
    className={`block w-full font-mono ${className}`}
  >
    {/* Diagonal striped fill for the WASP CLI frame. */}
    <defs>
      <pattern
        id="waspCliStripes"
        width="8"
        height="8"
        patternUnits="userSpaceOnUse"
        patternTransform="rotate(45)"
      >
        <rect width="8" height="8" className={styles.stripeBackground} />
        <line
          x1="0"
          y1="0"
          x2="0"
          y2="8"
          className={styles.stripeLine}
          strokeWidth="4"
        />
      </pattern>
    </defs>

    {/* ─── 1. Your code ─── */}
    <g>
      <rect
        x="10"
        y="38"
        width="200"
        height="204"
        className={`${styles.surface} ${styles.inkStroke}`}
        strokeWidth="1.25"
      />
      <rect
        x="10"
        y="38"
        width="200"
        height="26"
        className={`${styles.yellow} ${styles.inkStroke}`}
        strokeWidth="1.25"
      />
      <text
        x="24"
        y="56"
        fontSize="10"
        fontWeight="bold"
        className={styles.onYellow}
        letterSpacing="2"
      >
        YOUR CODE
      </text>

      {/* *.wasp.ts — same size as the rest, just bold + yellow highlight */}
      <rect x="14" y="74" width="192" height="26" className={styles.yellow} />
      <g transform="translate(20, 79)">
        <WaspIcon />
      </g>
      <text
        x="44"
        y="91"
        fontSize="12"
        fontWeight="bold"
        className={styles.onYellow}
      >
        *.wasp.ts
      </text>

      {/* Other files — same indent so they read as "all at the same level" */}
      <g transform="translate(20, 107)">
        <ReactIcon />
      </g>
      <text x="44" y="119" fontSize="12" className={styles.text}>
        *.tsx
      </text>

      <g transform="translate(20, 135)">
        <NodeIcon />
      </g>
      <text x="44" y="147" fontSize="12" className={styles.text}>
        *.ts
      </text>

      <g transform="translate(20, 163)">
        <PrismaIcon />
      </g>
      <text x="44" y="175" fontSize="12" className={styles.text}>
        schema.prisma
      </text>

      <g transform="translate(20, 191)">
        <DockerIcon />
      </g>
      <text x="44" y="203" fontSize="12" className={styles.text}>
        Dockerfile
      </text>

      {/* …and any other source files */}
      <text x="44" y="231" fontSize="12" className={styles.text}>
        ...
      </text>
    </g>

    {/* ─── WASP CLI dashed frame (label is just text, no tab background) ─── */}
    <g>
      <rect
        x="275"
        y="38"
        width="536"
        height="204"
        fill="url(#waspCliStripes)"
        className={styles.inkStroke}
        strokeWidth="1.5"
        strokeDasharray="6 4"
      />
      <text
        x="318"
        y="30"
        fontSize="9"
        fontWeight="bold"
        className={styles.muted}
        letterSpacing="2"
      >
        WASP CLI
      </text>
    </g>

    {/* ─── arrow 1: COMPILE ─── centered in the YOUR CODE→IR gap, drawn over the frame ─── */}
    <g>
      {/* plate so the label stays legible over the frame's stripes */}
      <rect x="236" y="108" width="78" height="16" className={styles.plate} />
      <text
        x="275"
        y="120"
        textAnchor="middle"
        fontSize="10"
        className={styles.muted}
        letterSpacing="3"
        fontWeight="bold"
      >
        COMPILE
      </text>
      <line
        x1="244"
        y1="140"
        x2="294"
        y2="140"
        className={styles.inkStroke}
        strokeWidth="1.75"
      />
      <polygon points="294,133 306,140 294,147" className={styles.ink} />
    </g>

    {/* ─── 2. Wasp area: IR in its own grey box, label beneath the mark ─── */}
    <g>
      <rect
        x="339"
        y="100"
        width="104"
        height="80"
        className={`${styles.ir} ${styles.inkStroke}`}
        strokeWidth="1.5"
      />
      <text
        x="391"
        y="136"
        textAnchor="middle"
        fontSize="22"
        fontWeight="bold"
        className={styles.ink}
        letterSpacing="2"
      >
        IR
      </text>
      <text
        x="391"
        y="153"
        textAnchor="middle"
        fontSize="7.5"
        fontWeight="bold"
        className={styles.muted}
      >
        intermediate
      </text>
      <text
        x="391"
        y="164"
        textAnchor="middle"
        fontSize="7.5"
        fontWeight="bold"
        className={styles.muted}
      >
        representation
      </text>
    </g>

    {/* ─── arrow 2: GENERATE ─── */}
    <g>
      <text
        x="494"
        y="120"
        textAnchor="middle"
        fontSize="10"
        className={styles.muted}
        letterSpacing="3"
        fontWeight="bold"
      >
        GENERATE
      </text>
      <line
        x1="463"
        y1="140"
        x2="513"
        y2="140"
        className={styles.inkStroke}
        strokeWidth="1.75"
      />
      <polygon points="513,133 525,140 513,147" className={styles.ink} />
    </g>

    {/* ─── 3. Generated code — each tier in a colored bordered row ─── */}
    <g>
      <rect
        x="546"
        y="62"
        width="185"
        height="156"
        className={`${styles.surface} ${styles.inkStroke}`}
        strokeWidth="1.25"
      />
      <rect
        x="546"
        y="62"
        width="185"
        height="26"
        className={`${styles.yellow} ${styles.inkStroke}`}
        strokeWidth="1.25"
      />
      <text
        x="560"
        y="80"
        fontSize="10"
        fontWeight="bold"
        className={styles.onYellow}
        letterSpacing="2"
      >
        GENERATED CODE
      </text>
      {/* small lock — generated code is read-only */}
      <g transform="translate(712, 68)">
        <path
          d="M 3 6 L 3 4 A 3 3 0 0 1 9 4 L 9 6"
          fill="none"
          className={styles.onYellowStroke}
          strokeWidth="1.5"
          strokeLinecap="round"
        />
        <rect x="1" y="6" width="10" height="8" className={styles.onYellow} />
      </g>

      {/* frontend — React cyan (brand colours) */}
      <rect
        x="558"
        y="98"
        width="161"
        height="30"
        className={styles.frontend}
        stroke="#61DAFB"
        strokeWidth="1.5"
      />
      <text
        x="566"
        y="118"
        fontSize="11"
        fontWeight="bold"
        className={styles.ink}
      >
        frontend
      </text>
      <text
        x="713"
        y="118"
        textAnchor="end"
        fontSize="9"
        className={styles.muted}
      >
        React
      </text>

      {/* backend — Node green (brand colours) */}
      <rect
        x="558"
        y="138"
        width="161"
        height="30"
        className={styles.backend}
        stroke="#5FA04E"
        strokeWidth="1.5"
      />
      <text
        x="566"
        y="158"
        fontSize="11"
        fontWeight="bold"
        className={styles.ink}
      >
        backend
      </text>
      <text
        x="713"
        y="158"
        textAnchor="end"
        fontSize="9"
        className={styles.muted}
      >
        Node
      </text>

      {/* database — Prisma slate */}
      <rect
        x="558"
        y="178"
        width="161"
        height="30"
        strokeWidth="1.5"
        className={`${styles.surface} ${styles.database}`}
      />
      <text
        x="566"
        y="198"
        fontSize="11"
        fontWeight="bold"
        className={styles.ink}
      >
        database
      </text>
      <text
        x="713"
        y="198"
        textAnchor="end"
        fontSize="9"
        className={styles.muted}
      >
        Prisma
      </text>
    </g>

    {/* ─── arrow 3: DEPLOY ─── centered in the GENERATED→globe gap, drawn over the frame ─── */}
    <g>
      {/* plate so the label stays legible over the frame's stripes */}
      <rect x="778" y="108" width="66" height="16" className={styles.plate} />
      <text
        x="811"
        y="120"
        textAnchor="middle"
        fontSize="10"
        className={styles.muted}
        letterSpacing="3"
        fontWeight="bold"
      >
        DEPLOY
      </text>
      <line
        x1="780"
        y1="140"
        x2="830"
        y2="140"
        className={styles.inkStroke}
        strokeWidth="1.75"
      />
      <polygon points="830,133 842,140 830,147" className={styles.ink} />
    </g>

    {/* ─── 4. Anywhere — schematic globe, no surrounding box ─── */}
    <g transform="translate(890, 105)">
      <circle
        cx="35"
        cy="35"
        r="33"
        className={`${styles.surface} ${styles.inkStroke}`}
        strokeWidth="1.25"
      />
      {/* latitudes */}
      <line
        x1="6"
        y1="22"
        x2="64"
        y2="22"
        className={styles.inkStroke}
        strokeWidth="1.3"
      />
      <line
        x1="2"
        y1="35"
        x2="68"
        y2="35"
        className={styles.inkStroke}
        strokeWidth="1.3"
      />
      <line
        x1="6"
        y1="48"
        x2="64"
        y2="48"
        className={styles.inkStroke}
        strokeWidth="1.3"
      />
      {/* longitudes */}
      <path
        d="M 35 2 Q 17 35 35 68"
        fill="none"
        className={styles.inkStroke}
        strokeWidth="1.3"
      />
      <path
        d="M 35 2 Q 53 35 35 68"
        fill="none"
        className={styles.inkStroke}
        strokeWidth="1.3"
      />
      <line
        x1="35"
        y1="2"
        x2="35"
        y2="68"
        className={styles.inkStroke}
        strokeWidth="1.3"
      />
    </g>
    <text
      x="925"
      y="195"
      textAnchor="middle"
      fontSize="9"
      className={styles.muted}
    >
      Fly · Railway · your VPS
    </text>
  </svg>
);

/* Wasp mark: the actual brand SVG referenced from /static. */
const WaspIcon = () => (
  <image
    href={useBaseUrl("/img/wasp-logo.svg")}
    x="0"
    y="0"
    width="16"
    height="16"
  />
);

/* React mark: cyan orbital atom — three crossed ellipses + center dot. */
const ReactIcon = () => (
  <>
    <ellipse
      cx="8"
      cy="8"
      rx="7.5"
      ry="3"
      fill="none"
      stroke="#61DAFB"
      strokeWidth="1.5"
    />
    <ellipse
      cx="8"
      cy="8"
      rx="7.5"
      ry="3"
      fill="none"
      stroke="#61DAFB"
      strokeWidth="1.5"
      transform="rotate(60, 8, 8)"
    />
    <ellipse
      cx="8"
      cy="8"
      rx="7.5"
      ry="3"
      fill="none"
      stroke="#61DAFB"
      strokeWidth="1.5"
      transform="rotate(-60, 8, 8)"
    />
    <circle cx="8" cy="8" r="1.6" fill="#61DAFB" />
  </>
);

/* Node mark: filled green hexagon — Node's iconic silhouette. */
const NodeIcon = () => (
  <polygon points="8,1 14,4.5 14,11.5 8,15 2,11.5 2,4.5" fill="#5FA04E" />
);

/* Prisma mark: the official Prisma symbol from their press kit, inlined so
   it can switch colour with the color scheme. */
const PrismaIcon = () => (
  <svg width="16" height="16" viewBox="0 0 58 72">
    <path
      className={styles.prismaLogo}
      fillRule="evenodd"
      clipRule="evenodd"
      d="m.522473 45.0933c-.706664 1.1527-.695727 2.6071.028192 3.749l13.102735 20.6691c.8504 1.3415 2.4895 1.9532 4.0108 1.4968l37.8114-11.3434c2.0634-.6191 3.1016-2.9209 2.1997-4.8774l-24.3069-52.72733c-1.1854-2.571393-4.7589-2.782464-6.2388-.3685zm32.200027-30.9792c-.5166-1.1954-2.266-1.0113-2.5224.2655l-9.3581 46.5953c-.1973.9825.7226 1.8215 1.6828 1.5349l26.1246-7.7984c.7625-.2276 1.1553-1.0699.8397-1.8003z"
    />
  </svg>
);

/* Docker mark: the official Docker brand SVG. */
const DockerIcon = () => (
  <image
    href={useBaseUrl("/img/site/logos/docker-logo.svg")}
    x="0"
    y="0"
    width="16"
    height="16"
  />
);

export default HowItWorksDiagram;
