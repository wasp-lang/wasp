/* Compile pipeline diagram for the "How it works" section.

   Layout discipline:
   - Arrow rail at y=140 — content centers on this line.
   - Outer boxes (Code, frame, Hosts) all y=60-220 (160 tall).
   - Generated box inset 20px inside frame top/bottom.
   - All arrows have ~20px padding from neighbouring elements for the same feel.
   - File rows in "your code" all share the same indent — flat hierarchy. */
const HowItWorksDiagram = () => (
  <svg
    viewBox="0 0 1000 260"
    preserveAspectRatio="xMidYMid meet"
    aria-label="Wasp compiles your code into an intermediate representation, then generates a full-stack app (frontend, backend, database) ready to deploy anywhere."
    className="mx-auto block w-full max-w-5xl font-mono"
  >
    {/* ─── 1. Your code ─── */}
    <g>
      <rect x="10" y="60" width="200" height="160" fill="#FFF3CC" stroke="#111" strokeWidth="2.5" />
      <rect x="10" y="60" width="200" height="26" fill="#F5C842" stroke="#111" strokeWidth="2.5" />
      <text x="24" y="78" fontSize="12" fontWeight="bold" fill="#111" letterSpacing="2">
        YOUR CODE
      </text>

      {/* *.wasp.ts — highlighted row (still special, just same indent as the rest) */}
      <rect x="14" y="96" width="192" height="26" fill="#F5C842" />
      <g transform="translate(20, 100)">
        <WaspIcon />
      </g>
      <text x="44" y="114" fontSize="16" fontWeight="bold" fill="#111">
        *.wasp.ts
      </text>

      {/* Other files — same indent so they read as "all at the same level" */}
      <g transform="translate(20, 128)">
        <ReactIcon />
      </g>
      <text x="44" y="140" fontSize="14" fill="#333">*.tsx</text>

      <g transform="translate(20, 148)">
        <NodeIcon />
      </g>
      <text x="44" y="160" fontSize="14" fill="#333">*.ts</text>

      <g transform="translate(20, 168)">
        <PrismaIcon />
      </g>
      <text x="44" y="180" fontSize="14" fill="#333">schema.prisma</text>

      <g transform="translate(20, 188)">
        <DockerIcon />
      </g>
      <text x="44" y="200" fontSize="14" fill="#333">Dockerfile</text>
    </g>

    {/* ─── arrow 1: COMPILE ─── line length matches GENERATE for consistent feel */}
    <g>
      <text x="260" y="120" textAnchor="middle" fontSize="12" fill="#555" letterSpacing="3" fontWeight="bold">
        COMPILE
      </text>
      <line x1="229" y1="140" x2="279" y2="140" stroke="#111" strokeWidth="2.5" />
      <polygon points="279,133 291,140 279,147" fill="#111" />
    </g>

    {/* ─── WASP CLI dashed frame (label is just text, no tab background) ─── */}
    <g>
      <rect x="310" y="60" width="450" height="160" fill="none" stroke="#111" strokeWidth="2" strokeDasharray="6 4" />
      <text x="318" y="52" fontSize="11" fontWeight="bold" fill="#555" letterSpacing="2">
        WASP CLI
      </text>
    </g>

    {/* ─── 2. Wasp area: bare two-line label, no box, no header ─── */}
    <g>
      <text x="402" y="135" textAnchor="middle" fontSize="13" fontWeight="bold" fill="#111">
        intermediate
      </text>
      <text x="402" y="153" textAnchor="middle" fontSize="13" fontWeight="bold" fill="#111">
        representation
      </text>
    </g>

    {/* ─── arrow 2: GENERATE ─── */}
    <g>
      <text x="508" y="120" textAnchor="middle" fontSize="12" fill="#555" letterSpacing="3" fontWeight="bold">
        GENERATE
      </text>
      <line x1="477" y1="140" x2="527" y2="140" stroke="#111" strokeWidth="2.5" />
      <polygon points="527,133 539,140 527,147" fill="#111" />
    </g>

    {/* ─── 3. Generated code — each tier in a colored bordered row ─── */}
    <g>
      <rect x="560" y="80" width="185" height="120" fill="#FFF3CC" stroke="#111" strokeWidth="2.5" />
      <rect x="560" y="80" width="185" height="26" fill="#F5C842" stroke="#111" strokeWidth="2.5" />
      <text x="574" y="98" fontSize="12" fontWeight="bold" fill="#111" letterSpacing="2">
        GENERATED CODE
      </text>
      {/* small lock — generated code is read-only */}
      <g transform="translate(726, 86)">
        <path d="M 3 6 L 3 4 A 3 3 0 0 1 9 4 L 9 6" fill="none" stroke="#111" strokeWidth="1.5" strokeLinecap="round" />
        <rect x="1" y="6" width="10" height="8" fill="#111" />
      </g>

      {/* frontend — React cyan */}
      <rect x="572" y="114" width="161" height="22" fill="#E6F7FB" stroke="#61DAFB" strokeWidth="1.5" />
      <text x="580" y="129" fontSize="13" fontWeight="bold" fill="#111">frontend</text>
      <text x="727" y="129" textAnchor="end" fontSize="11" fill="#555">React</text>

      {/* backend — Node green */}
      <rect x="572" y="140" width="161" height="22" fill="#E8F5E0" stroke="#5FA04E" strokeWidth="1.5" />
      <text x="580" y="155" fontSize="13" fontWeight="bold" fill="#111">backend</text>
      <text x="727" y="155" textAnchor="end" fontSize="11" fill="#555">Node</text>

      {/* database — Prisma neutral */}
      <rect x="572" y="166" width="161" height="22" fill="#FAFAFA" stroke="#2D3748" strokeWidth="1.5" />
      <text x="580" y="181" fontSize="13" fontWeight="bold" fill="#111">database</text>
      <text x="727" y="181" textAnchor="end" fontSize="11" fill="#555">Prisma</text>
    </g>

    {/* ─── arrow 3: DEPLOY ─── equal padding from frame and globe */}
    <g>
      <text x="826" y="120" textAnchor="middle" fontSize="12" fill="#555" letterSpacing="3" fontWeight="bold">
        DEPLOY
      </text>
      <line x1="795" y1="140" x2="845" y2="140" stroke="#111" strokeWidth="2.5" />
      <polygon points="845,133 857,140 845,147" fill="#111" />
    </g>

    {/* ─── 4. Anywhere — schematic globe, no surrounding box ─── */}
    <g transform="translate(890, 95)">
      <circle cx="35" cy="35" r="33" fill="#FFF3CC" stroke="#111" strokeWidth="2.5" />
      {/* latitudes */}
      <line x1="6" y1="22" x2="64" y2="22" stroke="#111" strokeWidth="1.3" />
      <line x1="2" y1="35" x2="68" y2="35" stroke="#111" strokeWidth="1.3" />
      <line x1="6" y1="48" x2="64" y2="48" stroke="#111" strokeWidth="1.3" />
      {/* longitudes */}
      <path d="M 35 2 Q 17 35 35 68" fill="none" stroke="#111" strokeWidth="1.3" />
      <path d="M 35 2 Q 53 35 35 68" fill="none" stroke="#111" strokeWidth="1.3" />
      <line x1="35" y1="2" x2="35" y2="68" stroke="#111" strokeWidth="1.3" />
    </g>
    <text x="925" y="195" textAnchor="middle" fontSize="10" fill="#555">
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
    <ellipse cx="8" cy="8" rx="7.5" ry="3" fill="none" stroke="#61DAFB" strokeWidth="1.5" />
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
  <polygon
    points="8,1 14,4.5 14,11.5 8,15 2,11.5 2,4.5"
    fill="#5FA04E"
  />
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
