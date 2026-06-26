import jsyaml from 'js-yaml';

export const writeYAMLImpl = value =>
  jsyaml.dump(value, { indent: 2, sortKeys: false, lineWidth: -1 });
