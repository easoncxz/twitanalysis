import React from 'react';

import { pretty } from '../utils/utils';
import * as twitter from '../twitter/models';
import * as core from '../core';
import { t } from '../twitter/models';
import { RemoteData } from '../utils/remote-data';
import { typecheckNever, fetchJson } from '../utils/utils';

type MyDispatch<T> = (_: T) => T;

type Model = RemoteData<twitter.User, Error>;
type Msg = RemoteData<twitter.User, Error>;

export class Effects {
  constructor(private readonly dispatch: MyDispatch<core.Msg>) {}

  private wrap(small: Msg): core.Msg {
    return {
      type: 'fetch_me',
      sub: small,
    };
  }

  fetchMe(): core.Msg {
    fetchJson(t('account/verify_credentials'))
      .then(twitter.parseUser)
      .then(
        (user: twitter.User) => {
          this.dispatch(
            this.wrap({
              type: 'ok',
              data: user,
            }),
          );
        },
        (e) => this.dispatch(this.wrap({ type: 'error', error: e })),
      );
    return this.wrap({
      type: 'loading',
    });
  }
}

export const View: React.FC<{
  model: Model;
  dispatch: MyDispatch<core.Msg>;
}> = ({ model, dispatch }) => {
  const effects = new Effects(dispatch);
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
              return (
                <div>
                  <p>Error: {model.error.message}</p>
                  <button onClick={() => dispatch(effects.fetchMe())}>
                    Try again
                  </button>
                </div>
              );
            default:
              typecheckNever(model);
              throw new Error(`never: ${model}`);
          }
        })()}
      </div>
    </div>
  );
};
