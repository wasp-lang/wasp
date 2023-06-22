export default {
  transform: { '^.+\\.ts?$': 'ts-jest' },
  testEnvironment: 'node',
  testRegex: '/test/.*\\.test\\.ts$',
  moduleFileExtensions: ['ts', 'js'],
}
