const fs = require('fs');
const path = require('path');
const execSync = require('child_process').execSync;
const readline = require('readline').createInterface({
  input: process.stdin,
  output: process.stdout
});

// To run this script, position yourself in the parent dir of your wasp project (so one level above your wasp project), and run:
//   node migrate-0-11.js <your-wasp-project-dir-name>
// Make sure that you have wasp version 0.12 installed.

async function migrate() {
  const WASP_BIN = process.env['USE_WASP_CLI'] ? 'wasp-cli' : 'wasp';  // When developing this script, we can set USE_WASP_CLI.

  const stdout = execSync(`${WASP_BIN} version`);
  if (!stdout.toString().includes('0.12.')) {
    console.error('Error: Your wasp is not version 0.12.x');
    process.exit(1);
  }

  const projectDirName = process.argv[2].replace(/\/$/, '');
  if (projectDirName.split(path.sep).length > 1) {
    console.error('Error: Please provide the name of the project, not the path.');
    process.exit(1);
  }

  const answer = await readlineP(
    'This script will modify files on the disk. We strongly recommend you have a'
      + ' way to revert them back if needed (e.g. they are version controled (git)).'
      + ' If you want to continue, type Y:'
  );
  if (answer.trim().toUpperCase() !== 'Y') {
    console.log('Aborting...');
    process.exit(0);
  }

  const oldProjectDirName = projectDirName + '__old';

  console.log(`1. Renaming ${projectDirName} to ${oldProjectDirName}...`);
  fs.renameSync(projectDirName, oldProjectDirName);

  console.log(`2. Creating new project ${projectDirName}...`);
  execSync(`${WASP_BIN} new ${projectDirName}`, { stdio: 'inherit' });

  console.log(`3. Deleting all files of the ${projectDirName}/src except for vite-env.d.ts...`);
  const srcDir = path.join(projectDirName, 'src');
  fs.readdirSync(srcDir).forEach((file) => {
    if (file !== 'vite-env.d.ts') {
      fs.unlinkSync(path.join(srcDir, file));
    }
  });

  console.log(`4. Copying public dir from ${oldProjectDirName} to ${projectDirName}...`);
  const oldPublicDir = path.join(oldProjectDirName, 'src', 'client', 'public');
  const publicDir = path.join(projectDirName, 'public');
  if (fs.existsSync(oldPublicDir)) {
    fs.cpSync(oldPublicDir, publicDir, { recursive: true, overwrite: true });
  }

  console.log(`5. Copying the contents of ${oldProjectDirName}/src into ${projectDirName}/src...`);
  fs.cpSync(path.join(oldProjectDirName, 'src'), path.join(projectDirName, 'src'), { recursive: true, overwrite: false });

  console.log(`6. Deleting redundant files and folders from ${projectDirName}/src...`);
  ['.waspignore',
    path.join('client', 'vite-env.d.ts'),
    path.join('client', 'tsconfig.json'),
    path.join('server', 'tsconfig.json'),
    path.join('shared', 'tsconfig.json'),
  ].forEach((filePathInSrc) => {
    fs.unlinkSync(path.join(srcDir, filePathInSrc));
  })
  // Delete client/public if it exists.
  const publicDirInSrc = path.join(srcDir, 'client', 'public');
  if (fs.existsSync(publicDirInSrc)) {
    fs.rmSync(publicDirInSrc, { recursive: true });
  }

  console.log('7. Updating JS/TS imports from 0.11 to 0.12...');
  execSync(`npx jscodeshift@0.15.1 -t https://raw.githubusercontent.com/wasp-lang/wasp-codemod/main/src/transforms/imports-from-0-11-to-0-12.ts --extensions=js,ts,jsx,tsx ${srcDir}`, { stdio: 'inherit' });

  console.log('8. Copying main.wasp from old project to new project...');
  fs.copyFileSync(path.join(oldProjectDirName, 'main.wasp'), path.join(projectDirName, 'main.wasp'));

  console.log('9. Updating main.wasp (0.11 -> 0.12, @server -> @src/server, @client -> @src/client)...');
  const mainWaspPath = path.join(projectDirName, 'main.wasp');
  const mainWaspContent = fs.readFileSync(mainWaspPath, 'utf8');
  const updatedMainWaspContent = mainWaspContent
        .replace(/\^0\.11\.\d+/g, '^0.12.0')
        .replace(/@server\//g, '@src/server/')
        .replace(/@client\//g, '@src/client/');
  fs.writeFileSync(mainWaspPath, updatedMainWaspContent);

  console.log('10. TODO: move the dependencies from main.wasp into package.json.');

  console.log('11. Merging .gitignore from old project into .gitignore in new project...');
  const oldGitignorePath = path.join(oldProjectDirName, '.gitignore');
  const newGitignorePath = path.join(projectDirName, '.gitignore');
  if (fs.existsSync(oldGitignorePath)) {
    const oldGitignoreLines = fs.readFileSync(oldGitignorePath, 'utf8').split('\n');
    const newGitignoreLines = fs.readFileSync(newGitignorePath, 'utf8').split('\n');
    const linesToAdd = oldGitignoreLines.filter((line) => !newGitignoreLines.includes(line));
    fs.appendFileSync(newGitignorePath, linesToAdd.join('\n'));

  }

  console.log('12. Copying the rest of relevant top level files.');
  fs.readdirSync(oldProjectDirName).forEach((item) => {
    if (!['.gitignore', 'main.wasp', 'src', '.wasp', '.waspinfo', 'node_modules'].includes(item)) {
      const oldItemPath = path.join(oldProjectDirName, item);
      const newItemPath = path.join(projectDirName, item);
      if (fs.lstatSync(oldItemPath).isDirectory()) {
        fs.cpSync(oldItemPath, newItemPath, { recursive: true });
      } else {
        fs.copyFileSync(oldItemPath, newItemPath);
      }
    }
  });

  console.log(`13. Cleaning up (wasp clean, deleting ${oldProjectDirName} dir)`);
  execSync(`cd ${projectDirName} && ${WASP_BIN} clean`);
  execSync(`rm -rf ${oldProjectDirName}`);

  console.log(
    'All done! Please inspect the changes we did, ideally with `git diff`, to make sure they make sense.'
    + ' Look out for any TODO comments we might have generated for the imports, as those need to be resolved manually.'
  );
}

async function readlineP(prompt) {
  return new Promise(r => readline.question(prompt, r));
}

migrate()
  .then(() => {
    process.exit(0)
  })
  .catch(e => {
    console.error(e.toString());
    process.exit(1);
  });
