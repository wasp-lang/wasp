import classNames from "classnames";
import type { ReactNode } from "react";

import styles from "./HowItWorksDiagram.module.css";

const DIAGRAM_DESCRIPTION =
  "Wasp compiles your code into an intermediate representation, then " +
  "generates a full-stack app (frontend, backend, database) ready to " +
  "deploy anywhere.";

const HowItWorksDiagram = () => (
  <div
    role="img"
    aria-label={DIAGRAM_DESCRIPTION}
    className="mx-auto my-6 flex max-w-[1144px] flex-col items-center font-body text-wasp-black xl:flex-row xl:items-stretch xl:justify-center"
  >
    <Card title="YOUR CODE">
      <Row
        active
        glyph={<img src="img/wasp-logo.svg" alt="" width={16} height={16} />}
        label="*.wasp.ts"
      />
      <Row glyph={<ReactGlyph />} label="*.tsx" />
      <Row glyph={<NodeGlyph />} label="*.ts" />
      <Row
        glyph={
          <img src="img/lp/prisma-logo.svg" alt="" width={16} height={16} />
        }
        label="schema.prisma"
      />
      <Row
        glyph={
          <img src="img/lp/docker-logo.svg" alt="" width={18} height={18} />
        }
        label="Dockerfile"
      />
    </Card>

    <Connector label="COMPILE" />

    <div
      className={classNames(
        "relative flex w-full max-w-[360px] flex-col items-center rounded-lg border-[2.5px] border-dashed border-wasp-black px-[30px] pb-[30px] pt-10 xl:w-auto xl:max-w-none xl:flex-row",
        styles.cliHatch,
      )}
    >
      <span className="absolute -top-[13px] left-[22px] bg-wasp-bg px-[10px] text-xs font-bold tracking-[4px] text-wasp-g6">
        WASP CLI
      </span>

      <div className="shrink-0 border-2 border-wasp-black bg-[var(--wasp-code-bg)] px-[18px] py-4 text-center text-[15px] font-medium italic leading-[1.45] shadow-[4px_4px_0_var(--wasp-yd)]">
        <span className="mb-[5px] block text-[11px] font-extrabold not-italic tracking-[3px] text-wasp-g6">
          IR
        </span>
        intermediate
        <br />
        representation
      </div>

      <Connector label="GENERATE" />

      <Card title="GENERATED CODE" action={<LockGlyph />}>
        <Tier name="frontend" tech="React" accent="border-l-[#61dafb]" />
        <Tier name="backend" tech="Node" accent="border-l-[#5fa04e]" />
        <Tier name="database" tech="Prisma" accent="border-l-[#2d3748]" />
      </Card>
    </div>

    <Connector label="DEPLOY" />

    <div className="flex shrink-0 flex-col items-center gap-[14px] pl-2 xl:self-center">
      <span className="flex h-24 w-24 items-center justify-center rounded-full border-2 border-wasp-black bg-wasp-yellow-light shadow-[6px_6px_0_#1111111a]">
        <GlobeGlyph />
      </span>
      <span className="text-[13px] tracking-[1px] text-wasp-g6">
        Fly · Railway · your VPS
      </span>
    </div>
  </div>
);

const Card = ({
  title,
  action,
  children,
}: {
  title: string;
  action?: ReactNode;
  children: ReactNode;
}) => (
  <div className="w-full max-w-[360px] overflow-hidden border-2 border-wasp-black bg-wasp-white shadow-[6px_6px_0_#1111111a] xl:w-auto xl:min-w-[244px] xl:max-w-none">
    <div className="flex items-center justify-between gap-2 border-b-2 border-wasp-black bg-wasp-yellow px-[14px] py-[11px] text-sm font-extrabold tracking-[2px]">
      <span>{title}</span>
      {action}
    </div>
    <div className="py-[10px]">{children}</div>
  </div>
);

const Row = ({
  glyph,
  label,
  active = false,
}: {
  glyph: ReactNode;
  label: string;
  active?: boolean;
}) => (
  <div
    className={classNames(
      "flex items-center gap-[11px] px-4 py-[9px] text-[15px]",
      active && "border-y-[1.5px] border-y-wasp-black bg-[#fde98a] font-bold",
    )}
  >
    <span className="flex w-5 shrink-0 justify-center">{glyph}</span>
    <span>{label}</span>
  </div>
);

const Tier = ({
  name,
  tech,
  accent,
}: {
  name: string;
  tech: string;
  accent: string;
}) => (
  <div
    className={classNames(
      "mx-[14px] mb-[9px] flex items-center justify-between border-y-[1.5px] border-l-[5px] border-r-[1.5px] border-y-wasp-black border-r-wasp-black bg-white px-3 py-[9px] text-[15px] first:mt-1",
      accent,
    )}
  >
    <span className="font-extrabold">{name}</span>
    <span className="text-[13px] tracking-[1px] text-wasp-g6">{tech}</span>
  </div>
);

const Connector = ({ label }: { label: string }) => (
  <div className="flex shrink-0 flex-col items-center gap-[10px] self-center py-[6px] xl:px-5 xl:py-0">
    <span className="text-xs font-bold tracking-[4px] text-wasp-g6">
      {label}
    </span>
    <svg
      className="rotate-90 xl:rotate-0"
      width="74"
      height="20"
      viewBox="0 0 74 20"
      fill="none"
      aria-hidden="true"
    >
      <line x1="0" y1="10" x2="62" y2="10" stroke="#111" strokeWidth="2.5" />
      <polygon points="60,3 72,10 60,17" fill="#111" />
    </svg>
  </div>
);

/* React mark: cyan orbital atom. */
const ReactGlyph = () => (
  <svg width="18" height="18" viewBox="0 0 16 16" aria-hidden="true">
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
      transform="rotate(60 8 8)"
    />
    <ellipse
      cx="8"
      cy="8"
      rx="7.5"
      ry="3"
      fill="none"
      stroke="#61DAFB"
      strokeWidth="1.5"
      transform="rotate(-60 8 8)"
    />
    <circle cx="8" cy="8" r="1.6" fill="#61DAFB" />
  </svg>
);

/* Node mark: filled green hexagon. */
const NodeGlyph = () => (
  <svg width="16" height="16" viewBox="0 0 16 16" aria-hidden="true">
    <polygon points="8,1 14,4.5 14,11.5 8,15 2,11.5 2,4.5" fill="#5FA04E" />
  </svg>
);

/* Lock mark: generated code is read-only. */
const LockGlyph = () => (
  <svg width="12" height="14" viewBox="0 0 12 14" aria-hidden="true">
    <path
      d="M3 6 V4 a3 3 0 0 1 6 0 V6"
      fill="none"
      stroke="#111"
      strokeWidth="1.5"
      strokeLinecap="round"
    />
    <rect x="1" y="6" width="10" height="8" fill="#111" />
  </svg>
);

/* Globe mark: schematic, deploy anywhere. */
const GlobeGlyph = () => (
  <svg
    width="58"
    height="58"
    viewBox="0 0 64 64"
    fill="none"
    stroke="#111"
    strokeWidth="2"
    aria-hidden="true"
  >
    <circle cx="32" cy="32" r="26" />
    <ellipse cx="32" cy="32" rx="11" ry="26" />
    <line x1="6" y1="32" x2="58" y2="32" />
    <path d="M11 19c12 7 30 7 42 0M11 45c12-7 30-7 42 0" />
  </svg>
);

export default HowItWorksDiagram;
