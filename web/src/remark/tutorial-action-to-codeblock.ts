import { visit } from 'unist-util-visit';
import { join } from 'path';
import type { Root } from 'mdast';
import type { MdxJsxFlowElement } from 'mdast-util-mdx';
import { findPatchFile, transformPatchFile } from '../utils/patch-transformer';

interface TutorialActionProps {
  id: string;
  action: string;
  lang?: string;
  title?: string;
}

function extractTutorialActionProps(node: MdxJsxFlowElement): TutorialActionProps | null {
  const props: TutorialActionProps = {
    id: '',
    action: 'apply-patch',
  };

  if (!node.attributes) return null;

  for (const attr of node.attributes) {
    if (attr.type === 'mdxJsxAttribute' && attr.name) {
      const value = attr.value;
      if (typeof value === 'string') {
        props[attr.name as keyof TutorialActionProps] = value;
      } else if (value && typeof value === 'object' && 'value' in value) {
        props[attr.name as keyof TutorialActionProps] = value.value as string;
      }
    }
  }

  return props.id ? props : null;
}

export function tutorialActionToCodeblock() {
  return (tree: Root, file: any) => {
    const basePath = file.dirname || process.cwd();
    const patchesDir = join(basePath, 'patches');

    visit(tree, 'mdxJsxFlowElement', (node: MdxJsxFlowElement, index, parent) => {
      if (node.name !== 'TutorialAction') return;

      const props = extractTutorialActionProps(node);
      if (!props || props.action !== 'apply-patch') return;

      const patchPath = findPatchFile(patchesDir, props.id);
      if (!patchPath) {
        console.error(`Patch file not found for id: ${props.id}`);
        return;
      }

      const transformedCode = transformPatchFile(patchPath, props.lang || 'ts');

      // Create a code block node
      const metaValues = [
        props.title && `title="${props.title}"`,
        "auto-js",
      ];
      const codeBlock = {
        type: 'code',
        lang: props.lang || 'tsx',
        meta: metaValues.filter(Boolean).join(' '),
        value: transformedCode,
      };

      if (parent && typeof index === 'number') {
        parent.children[index] = codeBlock as any;
      }
    });
  };
}