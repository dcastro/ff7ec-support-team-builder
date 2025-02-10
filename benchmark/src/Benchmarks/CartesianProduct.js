export function cartesian4(xs) {
    return xs.reduce((a, b) => a.flatMap((x) => b.map((y) => [...x, y])), [[]]);
}
