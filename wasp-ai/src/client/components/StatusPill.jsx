export function StatusPill({ children, status }) {
  const statusToClassName = {
    idle: "bg-gray-100 border-gray-300 text-gray-700",
    inProgress: "bg-sky-100 border-sky-300 text-sky-700",
    success: "bg-green-100 border-green-300 text-green-700",
    error: "bg-red-100 border-red-300 text-red-700",
    warning: "bg-yellow-100 border-yellow-300 text-yellow-700",
  };
  return (
    <div className="flex items-center">
      <span
        className={`text-center inline-flex items-center pl-3 pr-4 py-1.5 rounded-lg border ${statusToClassName[status]}`}
      >
        <span className="w-1.5 h-1.5 rounded-full mr-2 bg-current"></span>
        {children}
      </span>
    </div>
  );
}
