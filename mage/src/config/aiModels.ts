export const PLANNING_MODEL = "gpt-5";
export const CODING_MODEL = "gpt-5-mini";

// Per-million-token pricing for the coding model (gpt-5-mini).
// Used for cost estimation. The coding model handles the bulk of tokens,
// so its rates are a reasonable approximation of total cost.
export const USD_PER_MILLION_INPUT_TOKENS = 0.25;
export const USD_PER_MILLION_OUTPUT_TOKENS = 2.0;
