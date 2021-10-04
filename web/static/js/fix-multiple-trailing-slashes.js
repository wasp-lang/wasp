if (window && window.location && window.location.pathname.endsWith('//')) {
  // If there are multiple slashes at the end of the URL, we remove redundant ones
  // so that only one is left.
  // e.g. wasp-lang.dev/ will stay wasp-lang.dev/,
  //      wasp-lang.dev will stay wasp-lang.dev,
  // but  wasp-lang.dev// will become wasp-leng.dev/,
  // and  wasp-lang.dev/// will become wasp-lang.dev/.
  let pathname = window.location.pathname
  let n = 0 // Num trailing slashes.
  for (; n < pathname.length && pathname[pathname.length - 1 - n] == '/'; n++);
  window.history.replaceState('', '', pathname.substr(0, pathname.length - n + 1))
}
