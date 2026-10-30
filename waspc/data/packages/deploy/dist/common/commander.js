export function getFullCommandName(command) {
    const fullCommandNameParts = [command.name()];
    for (let parent = command.parent; parent; parent = parent.parent) {
        fullCommandNameParts.unshift(parent.name());
    }
    return fullCommandNameParts.join(" ");
}
//# sourceMappingURL=commander.js.map