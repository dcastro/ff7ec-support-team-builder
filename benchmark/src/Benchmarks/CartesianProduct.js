export function cartesian4(xs) {
    return xs.reduce((a, b) => a.flatMap((x) => b.map((y) => [...x, y])), [[]]);
}
export function cartesianFilter3(xs) {
    return xs.reduce((a, b) => a.flatMap((x) => b.flatMap((y) => {
        if (y % 2 !== 0) {
            return [];
        }
        else {
            return [[...x, y]];
        }
    })), [[]]);
}
