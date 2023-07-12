import { useMemo, useState } from "react";
import { CheckIcon, XMarkIcon } from "@heroicons/react/20/solid";
import { Loader } from "./Loader";

const DEFAULT_NUM_LOGS_TO_SHOW = 4;

export function Logs({ logs, status, onRetry }) {
  const [showAllLogs, setShowAllLogs] = useState(false);
  const visibleLogs = useMemo(() => {
    if (logs) {
      return showAllLogs ? logs : logs.slice(-1 * DEFAULT_NUM_LOGS_TO_SHOW);
    } else {
      return [];
    }
  }, [logs, showAllLogs]);

  function toggleShowAllLogs() {
    setShowAllLogs(!showAllLogs);
  }

  function getEmoji(log) {
    if (
      log.toLowerCase().includes("generated") ||
      log.toLowerCase().includes("fixed") ||
      log.toLowerCase().includes("added") ||
      log.toLowerCase().includes("updated")
    ) {
      return "✅";
    }
    if (log.toLowerCase().includes("done!")) {
      return "🎉";
    }
    if (
      log.toLowerCase().includes("error") ||
      log.toLowerCase().includes("fail") ||
      log.toLowerCase().includes("took too long")
    ) {
      return "❌";
    }
    if (log.toLowerCase().includes("warning")) {
      return "⚠️";
    }
    if (log.toLowerCase().includes("tokens usage")) {
      return "📊";
    }
    if (log.toLowerCase().endsWith("...")) {
      return "⌛️";
    }
    return "🤖";
  }

  return (
    logs && (
      <>
        <header className="relative big-box-dark mt-4 mb-8">
          {status === "success" && (
            <div className="absolute inset-0 bg-green-500 opacity-[.15] z-0"></div>
          )}
          {status === "error" || status === "cancelled" && (
            <div className="absolute inset-0 bg-red-500 opacity-[.15] z-0"></div>
          )}

          <div className="relative">
            <ToggleButton
              numberOfLogs={logs.length}
              showAllLogs={showAllLogs}
              toggleShowAllLogs={toggleShowAllLogs}
              status={status}
              className="block md:hidden mb-4 z-10"
            />
          </div>

          <div className="flex justify-between items-flex-start">
            <div className="flex-shrink-0 mr-3 mb-2 self-end">
              {(status === "inProgress" || status === "pending") &&
                <Loader />
              }
              {status === "success" && (
                <div className="status-icon bg-green-500">
                  <CheckIcon className="w-4 h-4 text-white" />
                </div>
              )}
              {status === "error" || status === "cancelled" && (
                <div className="status-icon bg-red-500">
                  <XMarkIcon className="w-4 h-4 text-white" />
                </div>
              )}
            </div>
            {logs && (
              <pre className="flex-1 overflow-x-auto z-10">
                {logs.length === 0 && "Waiting for logs..."}

                {visibleLogs.map((log, i) => (
                  <pre
                    key={i}
                    className="mb-2"
                    style={{
                      opacity: logs.length <= DEFAULT_NUM_LOGS_TO_SHOW || showAllLogs
                        ? 1
                        : (i + 1) * (1 / DEFAULT_NUM_LOGS_TO_SHOW),
                    }}
                  >
                    {getEmoji(log) + " "}
                    {log}{" "}
                    {i === (visibleLogs.length - 1) && (status === "error" || status === "cancelled" || status === "success") && (
                      <button onClick={onRetry} className="button gray xs">
                        Retry
                      </button>
                    )}
                  </pre>
                ))}
              </pre>
            )}

            <ToggleButton
              numberOfLogs={logs.length}
              showAllLogs={showAllLogs}
              toggleShowAllLogs={toggleShowAllLogs}
              status={status}
              className="flex-shrink-0 ml-3 hidden md:block z-10"
            />
          </div>
        </header>
      </>
    )
  );
}

function ToggleButton({
  numberOfLogs,
  showAllLogs,
  toggleShowAllLogs,
  status,
  className = "",
}) {
  return (
    numberOfLogs > DEFAULT_NUM_LOGS_TO_SHOW && (
      <div className={className}>
        <button onClick={toggleShowAllLogs} className="p-2 px-4 rounded-full bg-slate-700 hover:bg-slate-600 text-slate-300">
          {showAllLogs ? "Collapse the logs" : "Expand the logs"}
        </button>
      </div>
    )
  );
}
