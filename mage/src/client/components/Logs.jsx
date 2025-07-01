import { CheckIcon, XMarkIcon } from "@heroicons/react/20/solid";
import { useMemo, useState } from "react";
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
        <header className="big-box-dark relative">
          {status === "success" && (
            <div className="absolute inset-0 z-0 bg-green-500 opacity-[.15]"></div>
          )}
          {status === "error" ||
            (status === "cancelled" && (
              <div className="absolute inset-0 z-0 bg-red-500 opacity-[.15]"></div>
            ))}

          <div className="relative">
            <ToggleButton
              numberOfLogs={logs.length}
              showAllLogs={showAllLogs}
              toggleShowAllLogs={toggleShowAllLogs}
              status={status}
              className="z-10 mb-4 block md:hidden"
            />
          </div>

          <div className="items-flex-start flex justify-between">
            <div className="mb-2 mr-3 flex-shrink-0 self-end">
              {(status === "inProgress" || status === "pending") && <Loader />}
              {status === "success" && (
                <div className="status-icon bg-green-500">
                  <CheckIcon className="h-4 w-4 text-white" />
                </div>
              )}
              {status === "error" ||
                (status === "cancelled" && (
                  <div className="status-icon bg-red-500">
                    <XMarkIcon className="h-4 w-4 text-white" />
                  </div>
                ))}
            </div>
            {logs && (
              <pre className="z-10 flex-1 overflow-x-auto">
                {logs.length === 0 && "Waiting for logs..."}

                {visibleLogs.map((log, i) => (
                  <pre
                    key={i}
                    className="mb-2"
                    style={{
                      opacity:
                        logs.length <= DEFAULT_NUM_LOGS_TO_SHOW || showAllLogs
                          ? 1
                          : (i + 1) * (1 / DEFAULT_NUM_LOGS_TO_SHOW),
                    }}
                  >
                    {getEmoji(log) + " "}
                    {log}{" "}
                    {i === visibleLogs.length - 1 &&
                      (status === "error" ||
                        status === "cancelled" ||
                        status === "success") && (
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
              className="z-10 ml-3 hidden flex-shrink-0 md:block"
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
        <button
          onClick={toggleShowAllLogs}
          className="rounded-full bg-slate-700 p-2 px-4 text-slate-300 hover:bg-slate-600"
        >
          {showAllLogs ? "Collapse the logs" : "Expand the logs"}
        </button>
      </div>
    )
  );
}
