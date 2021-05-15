import ColorHash from 'color-hash'

export const getTagColor = (tagName) => {
  const colorHash = new ColorHash()
  return colorHash.hex(tagName)
}
