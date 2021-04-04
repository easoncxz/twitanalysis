import React from 'react';

import { pretty } from '../utils/utils';
import * as twitter from '../twitter/models';
import { t } from '../twitter/models';
import { RemoteData } from '../utils/remote-data';
import { typecheckNever, fetchJson } from '../utils/utils';

export type Model = RemoteData<twitter.User, Error>;

export type Msg = RemoteData<twitter.User, Error>;

export class Effects {
  constructor(private readonly dispatch: (_: Msg) => unknown) {}

  fetchMe(): Msg {
    fetchJson(t('account/verify_credentials'))
      .then(twitter.parseUser)
      .then(
        (user: twitter.User) => {
          this.dispatch({
            type: 'ok',
            data: user,
          });
        },
        (e) => this.dispatch({ type: 'error', error: e }),
      );
    return {
      type: 'loading',
    };
  }
}

export const View: React.FC<{
  model: Model;
  dispatch: (_: Msg) => void;
  effects: Effects;
}> = ({ model, dispatch, effects }) => {
  return (
    <div>
      <h1>Fetch own user</h1>
      <div>
        {(() => {
          switch (model.type) {
            case 'idle':
              return (
                <button onClick={() => dispatch(effects.fetchMe())}>
                  Tell me who I am
                </button>
              );
            case 'loading':
              return (
                <>
                  <p>Fetching...</p>
                  <button onClick={() => dispatch(effects.fetchMe())} disabled>
                    Tell me who I am
                  </button>
                </>
              );
            case 'ok':
              return (
                <div>
                  <p>You are:</p>
                  <pre>{pretty(model.data)}</pre>
                </div>
              );
            case 'error':
              return <div>Error: {model.error.message}</div>;
            default:
              typecheckNever(model);
              throw new Error(`never: ${model}`);
          }
        })()}
      </div>
    </div>
  );
};
