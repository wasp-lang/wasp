export function ListboxWrapper({ children }: { children: React.ReactNode }) {
  return (
    <div className="border-small px-1 py-2 rounded-small border-default-200 dark:border-default-100">
      {children}
    </div>
  );
}
