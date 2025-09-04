export const errorMessage = (e) => {
  return `Error: ${e.message} ${e.data?.message ? '- Details: ' + e.data.message : ''}`
}
