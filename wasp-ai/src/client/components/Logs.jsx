import { useMemo, useState } from "react";
import { CheckIcon, XMarkIcon } from "@heroicons/react/20/solid";
import { Loader } from "./Loader";

export function Logs({ logs, status, onRetry }) {
  const [logsVisible, setLogsVisible] = useState(false);
  const previewLogsCount = 3;
  const visibleLogs = useMemo(() => {
    if (logs) {
      return logsVisible ? logs : logs.slice(0, previewLogsCount);
    } else {
      return [];
    }
  }, [logs, logsVisible]);

  function toggleLogs() {
    setLogsVisible(!logsVisible);
  }

  function getEmoji(log) {
    // log.toLowerCase().includes("generated") ? "✅ " : "⌛️ "
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
      log.toLowerCase().includes("fail")
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
        <header className={`big-box mt-4 mb-4 ${status}`}>
          <ToggleButton
            numberOfLogs={logs.length}
            logsVisible={logsVisible}
            toggleLogs={toggleLogs}
            status={status}
            className="md:hidden mb-4"
          />
          <div className="flex justify-between items-flex-start">
            <div className="flex-shrink-0 mr-3">
              {(status === "inProgress" || status === "pending") && <Loader />}
              {status === "success" && (
                <div className="status-icon bg-green-500">
                  <CheckIcon className="w-4 h-4 text-white" />
                </div>
              )}
              {status === "error" && (
                <div className="status-icon bg-red-500">
                  <XMarkIcon className="w-4 h-4 text-white" />
                </div>
              )}
            </div>
            {logs && (
              <pre className="flex-1 overflow-x-auto">
                {logs.length === 0 && "Waiting for logs..."}

                {visibleLogs.map((log, i) => (
                  <pre
                    key={i}
                    className="mb-2"
                    style={{
                      opacity: logsVisible
                        ? 1
                        : (previewLogsCount - i) * (1 / previewLogsCount),
                    }}
                  >
                    {getEmoji(log) + " "}
                    {log}{" "}
                    {i === 0 && (status === "error" || status === "cancelled") && (
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
              logsVisible={logsVisible}
              toggleLogs={toggleLogs}
              status={status}
              className="flex-shrink-0 ml-3 hidden md:block"
            />
          </div>
        </header>
      </>
    )
  );
}

function ToggleButton({
  numberOfLogs,
  logsVisible,
  toggleLogs,
  status,
  className = "",
}) {
  return (
    numberOfLogs > 1 && (
      <div className={className}>
        <button onClick={toggleLogs} className={`big-box-button ${status}`}>
          {logsVisible ? "Collapse the logs" : "Expand the logs"}
        </button>
      </div>
    )
  );
}
