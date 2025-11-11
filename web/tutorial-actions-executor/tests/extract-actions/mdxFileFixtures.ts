export const mdxWithInitAppAction = `
# Tutorial

<TutorialAction id="init-project" action="INIT_APP" starterTemplateName="basic" />

Some content here.
`;

export const mdxWithApplyPatchAction = `
# Tutorial

<TutorialAction id="add-feature" action="APPLY_PATCH">

\`\`\`
const someCode = ""
\`\`\`
</TutorialAction>

Some content here.
`;

export const mdxWithMigrateDbAction = `
# Tutorial

<TutorialAction id="migrate-schema" action="MIGRATE_DB" />

Some content here.
`;

export const mdxWithMultipleActions = `
# Tutorial

<TutorialAction id="init-project" action="INIT_APP" starterTemplateName="basic" />

Some content here.

<TutorialAction id="add-feature" action="APPLY_PATCH">
\`\`\`
const someCode = ""
\`\`\`
</TutorialAction>

More content.

<TutorialAction id="migrate-schema" action="MIGRATE_DB" />
`;

export const mdxWithNoActions = `
# Tutorial

Just some regular content without any tutorial actions.
`;

export const mdxWithNonTutorialActionComponents = `
# Tutorial

<SomeOtherComponent id="test" />

<TutorialAction id="valid-action" action="MIGRATE_DB" />

<AnotherComponent action="SOMETHING" />
`;

export const mdxWithMissingActionAttribute = `
<TutorialAction id="test-action" />
`;

export const mdxWithMissingIdAttribute = `
<TutorialAction action="INIT_APP" starterTemplateName="basic" />
`;

export const mdxWithMissingStarterTemplateName = `
<TutorialAction id="init-project" action="INIT_APP" />
`;

export const mdxWithUnknownActionType = `
<TutorialAction id="test-action" action="UNKNOWN_ACTION" />
`;
