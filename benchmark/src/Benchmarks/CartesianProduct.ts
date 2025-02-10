export function cartesian4(xs: number[][]): number[][] {
  return xs.reduce(
    (a: number[][], b: number[]) =>
      a.flatMap((x: number[]) => b.map((y: number) => [...x, y])),
    [[]]
  );
}
