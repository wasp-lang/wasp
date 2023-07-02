export function StatusPill({ children, status, className = "" }) {
  const statusToClassName = {
    idle: "bg-gray-100 text-gray-700",
    inProgress: "bg-sky-100 text-sky-700",
    success: "bg-green-100 text-green-700",
    error: "bg-red-100 text-red-700",
    warning: "bg-yellow-100 text-yellow-700",
  };
  return (
    <div className={`flex items-center ${className}`}>
      <span
        className={`text-center inline-flex items-center pl-3 pr-4 py-2 rounded-lg shadow-md ${statusToClassName[status]}`}
      >
        <span className="w-1.5 h-1.5 rounded-full mr-2 bg-current"></span>
        {children}
      </span>
    </div>
  );
}
