import React from 'react';

/**
 * Help reading code -- all `useEffect` calls look too similar
 */
export function useInit(f: React.EffectCallback): void {
  return React.useEffect(f, []);
}
