import { spawn } from 'node:child_process';
export function typescriptCheck(options) {
    return {
        name: 'wasp:typescript-check',
        apply: 'build',
        async buildStart() {
            await runTsc(options.srcTsConfigPath);
        },
    };
}
function runTsc(srcTsConfigPath) {
    return new Promise((resolve, reject) => {
        const child = spawn('tsc', ['--project', srcTsConfigPath, '--noEmit'], {
            stdio: 'inherit',
            shell: process.platform === 'win32',
        });
        child.once('error', reject);
        child.once('close', (code) => code === 0
            ? resolve()
            : reject(new Error(`TypeScript check failed (exit ${code})`)));
    });
}
//# sourceMappingURL=typescriptCheck.js.map