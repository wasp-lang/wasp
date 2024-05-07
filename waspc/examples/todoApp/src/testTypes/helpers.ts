export type Assert<T extends { params: true; return: true }> = T
export type AreEqual<T, Expected> = T extends Expected
  ? Expected extends T
    ? true
    : false
  : false
