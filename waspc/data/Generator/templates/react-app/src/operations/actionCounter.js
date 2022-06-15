
const actionCounter = new Map()
window.actionCounter = actionCounter;

export function makeActionCounter() {
	function increment(queryCacheKey) {
		const current = actionCounter.get(queryCacheKey) ?? 0;
		actionCounter.set(queryCacheKey, current + 1)
		// todo: remove this
		console.log('increment action counter', queryCacheKey, actionCounter.get(queryCacheKey))
	}

	function decrement(queryCacheKey) {
		const current = actionCounter.get(queryCacheKey) ?? 0;
		actionCounter.set(queryCacheKey, current - 1)
		// todo: remove this
		console.log('decrement action counter', queryCacheKey, actionCounter.get(queryCacheKey))
	}

	function hasNoActionsInProgress(queryCacheKey) {
		const actionsInProgress = actionCounter.get(queryCacheKey) ?? 0
		return actionsInProgress === 0;
	}

	return {
		increment,
		decrement,
		hasNoActionsInProgress
	}
}