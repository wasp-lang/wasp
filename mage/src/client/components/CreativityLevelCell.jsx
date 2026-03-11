export function CreativityLevelCell({ level }) {
  return <td className={`px-6 py-4 creativity-${level}`}>{level ?? "-"}</td>;
}
