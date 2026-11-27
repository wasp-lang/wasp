module Wasp.Diagnostic
  ( Diagnostic (..),
    DiagnosticKind (..),
    makeError,
    makeWarning,
    makeSuccess,
    setDetails,
  )
where

data DiagnosticKind = Error | Warning | Success

data Diagnostic = Diagnostic
  { kind :: DiagnosticKind,
    title :: String,
    details :: Maybe String
  }

makeDiagnostic :: DiagnosticKind -> String -> Diagnostic
makeDiagnostic diagnosticKind diagnosticTitle =
  Diagnostic {kind = diagnosticKind, title = diagnosticTitle, details = Nothing}

makeError :: String -> Diagnostic
makeError = makeDiagnostic Error

makeWarning :: String -> Diagnostic
makeWarning = makeDiagnostic Warning

makeSuccess :: String -> Diagnostic
makeSuccess = makeDiagnostic Success

setDetails :: String -> Diagnostic -> Diagnostic
setDetails newDetails diagnostic = diagnostic {details = Just newDetails}
