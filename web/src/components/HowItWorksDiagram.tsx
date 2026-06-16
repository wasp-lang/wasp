/* Compile pipeline diagram for the "How it works" section.

   Layout discipline:
   - Arrow rail at y=140 — every stage (boxes, arrows, globe) centers on it.
   - The YOUR CODE box and the WASP CLI frame share the same extent (y=38-242).
   - COMPILE and DEPLOY are centered on the frame's left/right edges, so each
     arrow sits half inside the frame and half outside; a white plate keeps the
     label legible where it crosses the striped fill.
   - File rows in "your code" all share the same indent — flat hierarchy.

   Colours come from the Wasp palette via Tailwind's fill and stroke utilities.
   The only raw hex left is genuinely off-palette: the React/Node brand colours
   and a few bespoke tints (stripe greys, coloured tier-row fills). */
const HowItWorksDiagram = () => (
  <svg
    viewBox="0 0 1000 260"
    preserveAspectRatio="xMidYMid meet"
    aria-label="Wasp compiles your code into an intermediate representation, then generates a full-stack app (frontend, backend, database) ready to deploy anywhere."
    className="block w-full font-mono"
  >
    {/* Diagonal striped fill for the WASP CLI frame — bespoke light greys. */}
    <defs>
      <pattern
        id="waspCliStripes"
        width="8"
        height="8"
        patternUnits="userSpaceOnUse"
        patternTransform="rotate(45)"
      >
        <rect width="8" height="8" fill="#F7F7F7" />
        <line x1="0" y1="0" x2="0" y2="8" stroke="#E2E2E2" strokeWidth="4" />
      </pattern>
    </defs>

    {/* ─── 1. Your code ─── */}
    <g>
      <rect
        x="10"
        y="38"
        width="200"
        height="204"
        className="fill-wasp-white stroke-wasp-black"
        strokeWidth="1.25"
      />
      <rect
        x="10"
        y="38"
        width="200"
        height="26"
        className="fill-wasp-yellow stroke-wasp-black"
        strokeWidth="1.25"
      />
      <text
        x="24"
        y="56"
        fontSize="12"
        fontWeight="bold"
        className="fill-wasp-black"
        letterSpacing="2"
      >
        YOUR CODE
      </text>

      {/* *.wasp.ts — same size as the rest, just bold + yellow highlight */}
      <rect
        x="14"
        y="74"
        width="192"
        height="26"
        className="fill-wasp-yellow"
      />
      <g transform="translate(20, 79)">
        <WaspIcon />
      </g>
      <text
        x="44"
        y="91"
        fontSize="14"
        fontWeight="bold"
        className="fill-wasp-black"
      >
        *.wasp.ts
      </text>

      {/* Other files — same indent so they read as "all at the same level" */}
      <g transform="translate(20, 107)">
        <ReactIcon />
      </g>
      <text x="44" y="119" fontSize="14" className="fill-wasp-g7">
        *.tsx
      </text>

      <g transform="translate(20, 135)">
        <NodeIcon />
      </g>
      <text x="44" y="147" fontSize="14" className="fill-wasp-g7">
        *.ts
      </text>

      <g transform="translate(20, 163)">
        <PrismaIcon />
      </g>
      <text x="44" y="175" fontSize="14" className="fill-wasp-g7">
        schema.prisma
      </text>

      <g transform="translate(20, 191)">
        <DockerIcon />
      </g>
      <text x="44" y="203" fontSize="14" className="fill-wasp-g7">
        Dockerfile
      </text>

      {/* …and any other source files */}
      <text x="44" y="231" fontSize="14" className="fill-wasp-g7">
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
        className="stroke-wasp-black"
        strokeWidth="1.5"
        strokeDasharray="6 4"
      />
      <text
        x="318"
        y="30"
        fontSize="11"
        fontWeight="bold"
        className="fill-wasp-g6"
        letterSpacing="2"
      >
        WASP CLI
      </text>
    </g>

    {/* ─── arrow 1: COMPILE ─── centered in the YOUR CODE→IR gap, drawn over the frame ─── */}
    <g>
      {/* white plate so the label stays legible over the frame's stripes */}
      <rect x="236" y="108" width="78" height="16" className="fill-white" />
      <text
        x="275"
        y="120"
        textAnchor="middle"
        fontSize="12"
        className="fill-wasp-g6"
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
        className="stroke-wasp-black"
        strokeWidth="1.75"
      />
      <polygon points="294,133 306,140 294,147" className="fill-wasp-black" />
    </g>

    {/* ─── 2. Wasp area: IR in its own grey box, label beneath the mark ─── */}
    <g>
      <rect
        x="339"
        y="100"
        width="104"
        height="80"
        fill="#E8E8E8"
        className="stroke-wasp-black"
        strokeWidth="1.5"
      />
      <text
        x="391"
        y="136"
        textAnchor="middle"
        fontSize="28"
        fontWeight="bold"
        className="fill-wasp-black"
        letterSpacing="2"
      >
        IR
      </text>
      <text
        x="391"
        y="153"
        textAnchor="middle"
        fontSize="8.5"
        fontWeight="bold"
        className="fill-wasp-g6"
      >
        intermediate
      </text>
      <text
        x="391"
        y="164"
        textAnchor="middle"
        fontSize="8.5"
        fontWeight="bold"
        className="fill-wasp-g6"
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
        fontSize="12"
        className="fill-wasp-g6"
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
        className="stroke-wasp-black"
        strokeWidth="1.75"
      />
      <polygon points="513,133 525,140 513,147" className="fill-wasp-black" />
    </g>

    {/* ─── 3. Generated code — each tier in a colored bordered row ─── */}
    <g>
      <rect
        x="546"
        y="62"
        width="185"
        height="156"
        className="fill-wasp-white stroke-wasp-black"
        strokeWidth="1.25"
      />
      <rect
        x="546"
        y="62"
        width="185"
        height="26"
        className="fill-wasp-yellow stroke-wasp-black"
        strokeWidth="1.25"
      />
      <text
        x="560"
        y="80"
        fontSize="12"
        fontWeight="bold"
        className="fill-wasp-black"
        letterSpacing="2"
      >
        GENERATED CODE
      </text>
      {/* small lock — generated code is read-only */}
      <g transform="translate(712, 68)">
        <path
          d="M 3 6 L 3 4 A 3 3 0 0 1 9 4 L 9 6"
          fill="none"
          className="stroke-wasp-black"
          strokeWidth="1.5"
          strokeLinecap="round"
        />
        <rect x="1" y="6" width="10" height="8" className="fill-wasp-black" />
      </g>

      {/* frontend — React cyan (brand colours) */}
      <rect
        x="558"
        y="98"
        width="161"
        height="30"
        fill="#E6F7FB"
        stroke="#61DAFB"
        strokeWidth="1.5"
      />
      <text
        x="566"
        y="118"
        fontSize="13"
        fontWeight="bold"
        className="fill-wasp-black"
      >
        frontend
      </text>
      <text
        x="713"
        y="118"
        textAnchor="end"
        fontSize="11"
        className="fill-wasp-g6"
      >
        React
      </text>

      {/* backend — Node green (brand colours) */}
      <rect
        x="558"
        y="138"
        width="161"
        height="30"
        fill="#E8F5E0"
        stroke="#5FA04E"
        strokeWidth="1.5"
      />
      <text
        x="566"
        y="158"
        fontSize="13"
        fontWeight="bold"
        className="fill-wasp-black"
      >
        backend
      </text>
      <text
        x="713"
        y="158"
        textAnchor="end"
        fontSize="11"
        className="fill-wasp-g6"
      >
        Node
      </text>

      {/* database — Prisma slate */}
      <rect
        x="558"
        y="178"
        width="161"
        height="30"
        stroke="#2D3748"
        strokeWidth="1.5"
        className="fill-wasp-white"
      />
      <text
        x="566"
        y="198"
        fontSize="13"
        fontWeight="bold"
        className="fill-wasp-black"
      >
        database
      </text>
      <text
        x="713"
        y="198"
        textAnchor="end"
        fontSize="11"
        className="fill-wasp-g6"
      >
        Prisma
      </text>
    </g>

    {/* ─── arrow 3: DEPLOY ─── centered in the GENERATED→globe gap, drawn over the frame ─── */}
    <g>
      {/* white plate so the label stays legible over the frame's stripes */}
      <rect x="778" y="108" width="66" height="16" className="fill-white" />
      <text
        x="811"
        y="120"
        textAnchor="middle"
        fontSize="12"
        className="fill-wasp-g6"
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
        className="stroke-wasp-black"
        strokeWidth="1.75"
      />
      <polygon points="830,133 842,140 830,147" className="fill-wasp-black" />
    </g>

    {/* ─── 4. Anywhere — schematic globe, no surrounding box ─── */}
    <g transform="translate(890, 105)">
      <circle
        cx="35"
        cy="35"
        r="33"
        className="fill-wasp-white stroke-wasp-black"
        strokeWidth="1.25"
      />
      {/* latitudes */}
      <line
        x1="6"
        y1="22"
        x2="64"
        y2="22"
        className="stroke-wasp-black"
        strokeWidth="1.3"
      />
      <line
        x1="2"
        y1="35"
        x2="68"
        y2="35"
        className="stroke-wasp-black"
        strokeWidth="1.3"
      />
      <line
        x1="6"
        y1="48"
        x2="64"
        y2="48"
        className="stroke-wasp-black"
        strokeWidth="1.3"
      />
      {/* longitudes */}
      <path
        d="M 35 2 Q 17 35 35 68"
        fill="none"
        className="stroke-wasp-black"
        strokeWidth="1.3"
      />
      <path
        d="M 35 2 Q 53 35 35 68"
        fill="none"
        className="stroke-wasp-black"
        strokeWidth="1.3"
      />
      <line
        x1="35"
        y1="2"
        x2="35"
        y2="68"
        className="stroke-wasp-black"
        strokeWidth="1.3"
      />
    </g>
    <text
      x="925"
      y="195"
      textAnchor="middle"
      fontSize="10"
      className="fill-wasp-g6"
    >
      Fly · Railway · your VPS
    </text>
  </svg>
);

/* Wasp mark: the actual brand SVG referenced from /static. */
const WaspIcon = () => (
  <image href="img/wasp-logo.svg" x="0" y="0" width="16" height="16" />
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

/* Prisma mark: the official Prisma symbol from their press kit. */
const PrismaIcon = () => (
  <image href="img/lp/prisma-logo.svg" x="0" y="0" width="16" height="16" />
);

/* Docker mark: the official Docker brand SVG. */
const DockerIcon = () => (
  <image href="img/lp/docker-logo.svg" x="0" y="0" width="16" height="16" />
);

export default HowItWorksDiagram;
