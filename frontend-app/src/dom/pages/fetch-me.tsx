import React from 'react';

import { pretty } from '../utils/utils';
import * as core from '../core';
import * as effects from '../effects';

export const View: React.FC<{
  model: core.Model;
  dispatch: (_: core.Msg) => void;
  effects: effects.Effects;
}> = ({ model, dispatch, effects }) => {
  return (
    <div>
      <h1>Fetch own user</h1>
      {model.user ? (
        <div>
          <p>You are:</p>
          <pre>{pretty(model.user)}</pre>
        </div>
      ) : (
        <div>
          {model.fetchingMe ? <p>Fetching...</p> : null}
          <button
            onClick={() => dispatch(effects.fetchMe())}
            disabled={model.fetchingMe}
          >
            Tell me who I am
          </button>
        </div>
      )}
    </div>
  );
};
