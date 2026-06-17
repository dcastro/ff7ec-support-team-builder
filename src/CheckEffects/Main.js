import { readFileSync } from 'fs';

export const readWeaponsValues = path => () => {
  const data = JSON.parse(readFileSync(path, 'utf8'));
  return data.values ?? [];
};
