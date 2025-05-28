export const HookStatus = ({
  hookName,
  isCalled,
}: {
  hookName: string;
  isCalled: boolean;
}) => {
  return (
    <div className="flex items-center justify-between border p-3 rounded-lg shadow-sm">
      <span className="text-gray-600 text-sm">{hookName}</span>
      <span
        className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
          isCalled ? "bg-green-100 text-green-800" : "bg-gray-100 text-gray-800"
        }`}
      >
        {isCalled ? "Called" : "Not Called"}
      </span>
    </div>
  );
};
