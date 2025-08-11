import { readFileSync, readdirSync } from 'fs';
import { join } from 'path';
import parseGitDiff from 'parse-git-diff';

const LANGUAGE_COMMENTS = {
  javascript: '// ...',
  js: '// ...',
  typescript: '// ...',
  ts: '// ...',
  tsx: '// ...',
  jsx: '// ...',
  wasp: '// ...',
  css: '/* ... */',
  html: '<!-- ... -->',
  xml: '<!-- ... -->',
  json: '// ...',
  yaml: '# ...',
  yml: '# ...',
  python: '# ...',
  py: '# ...',
  shell: '# ...',
  bash: '# ...',
  prisma: '// ...',
} as const;

function getLanguageComment(lang: string): string {
  return LANGUAGE_COMMENTS[lang as keyof typeof LANGUAGE_COMMENTS] || '// ...';
}

export function transformPatch(patchContent: string, lang: string = 'ts'): string {
  try {
    const parsed = parseGitDiff(patchContent);
    
    if (!parsed.files.length) {
      throw new Error('No files found in patch');
    }

    // Focus on the main file (usually the first one with additions after deletions)
    const mainFile = parsed.files.find(file => 
      file.chunks.some(chunk => 
        'changes' in chunk && chunk.changes.some(change => change.type === 'AddedLine')
      )
    ) || parsed.files[0];

    const lines: string[] = [];
    const comment = getLanguageComment(lang);

    // Group additions by their logical sections
    const additionGroups: Array<{ changes: any[], startLine: number, endLine: number }> = [];
    
    for (const chunk of mainFile.chunks) {
      if (!('changes' in chunk)) continue;
      
      const additions = chunk.changes.filter(change => change.type === 'AddedLine');
      if (additions.length === 0) continue;
      
      // Group consecutive additions together
      let currentGroup = [];
      let lastLineNumber = -1;
      
      for (const change of chunk.changes) {
        if (change.type === 'AddedLine') {
          // If this addition is not consecutive to the last, start a new group
          if (lastLineNumber !== -1 && change.lineAfter > lastLineNumber + 5) {
            if (currentGroup.length > 0) {
              additionGroups.push({
                changes: currentGroup,
                startLine: currentGroup[0].lineAfter,
                endLine: currentGroup[currentGroup.length - 1].lineAfter
              });
              currentGroup = [];
            }
          }
          currentGroup.push(change);
          lastLineNumber = change.lineAfter;
        }
      }
      
      if (currentGroup.length > 0) {
        additionGroups.push({
          changes: currentGroup,
          startLine: currentGroup[0].lineAfter,
          endLine: currentGroup[currentGroup.length - 1].lineAfter
        });
      }
    }

    // Output each group
    for (let i = 0; i < additionGroups.length; i++) {
      const group = additionGroups[i];
      
      // Add context comment before if not the first group
      if (i > 0) {
        lines.push(comment);
      }
      
      // Add highlight around this group
      lines.push('// highlight-start');
      for (const change of group.changes) {
        lines.push(change.content);
      }
      lines.push('// highlight-end');
    }

    return lines.join('\n');
  } catch (error) {
    console.error(`Error parsing patch:`, error);
    return `// Error loading patch: ${error.message}`;
  }
}

export function findPatchFile(patchesDir: string, id: string): string | null {
  try {
    const patchFiles = readdirSync(patchesDir);
    
    // Look for a patch file that ends with __{id}.patch
    const matchingFile = patchFiles.find(file => {
      return file.endsWith(`__${id}.patch`);
    });

    if (matchingFile) {
      return join(patchesDir, matchingFile);
    }

    // If no match found, try direct id.patch as fallback
    const directPath = join(patchesDir, `${id}.patch`);
    try {
      readFileSync(directPath);
      return directPath;
    } catch {
      // File doesn't exist
    }

    return null;
  } catch (error) {
    console.error(`Error reading patches directory ${patchesDir}:`, error);
    return null;
  }
}

export function transformPatchFile(patchPath: string, lang: string = 'ts'): string {
  try {
    const patchContent = readFileSync(patchPath, 'utf-8');
    return transformPatch(patchContent, lang);
  } catch (error) {
    console.error(`Error reading patch file ${patchPath}:`, error);
    return `// Error loading patch: ${error.message}`;
  }
}