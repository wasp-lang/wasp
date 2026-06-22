export function LoadingSpinner() {
  return (
    <div className="flex items-center justify-center py-10">
      <div className="border-primary h-16 w-16 animate-spin rounded-full border-4 border-solid border-t-transparent"></div>
    </div>
  );
}
