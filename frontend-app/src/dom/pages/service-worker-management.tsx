import React from 'react';

import * as core from '../core';
import { Effects } from '../effects';

export const view = (
  dispatch: (_: core.Msg) => void,
  effects: Effects,
): React.ReactFragment => {
  return (
    <>
      <button onClick={() => dispatch(effects.registerServiceWorker())}>
        register ServiceWorker
      </button>
      <button onClick={() => dispatch(effects.unregisterAllServiceWorkers())}>
        unregister all ServiceWorkers
      </button>
    </>
  );
};
