export function cartesian4(xs: number[][]): number[][] {
  return xs.reduce(
    (a: number[][], b: number[]) =>
      a.flatMap((x: number[]) => b.map((y: number) => [...x, y])),
    [[]]
  );
}

export function cartesianFilter3(xs: number[][]): number[][] {
  return xs.reduce(
    (a: number[][], b: number[]) =>
      a.flatMap((x: number[]) =>
        b.flatMap((y: number) => {
          if (y % 2 !== 0) {
            return [];
          } else {
            return [[...x, y]];
          }
        })
      ),
    [[]]
  );
}
