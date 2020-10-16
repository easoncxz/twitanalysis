import type * as Redux from 'redux';

import { Msg } from './core';
import { User, Status, t } from './twitter';

export type Actions = {
  noop(): Msg;
  fetchMe(): Msg;
  sendTweet(t: string): Msg;
};

export const actionsOf = (dispatch: Redux.Dispatch<Msg>): Actions => ({
  noop() {
    return {
      type: 'noop',
    };
  },
  fetchMe() {
    fetch(t('account/verify_credentials.json'))
      .then((r) => r.json())
      .then((user: User) => {
        dispatch({ type: 'receive_fetch_me', user });
      });
    return {
      type: 'start_fetch_me',
    };
  },
  sendTweet(text: string) {
    const body = new URLSearchParams();
    body.set('status', text);
    fetch(t('statuses/update.json'), {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
      body,
    })
      .then((r) => r.json())
      .then((status: Status) => {
        dispatch({ type: 'receive_send_tweet', status });
      });
    return { type: 'start_send_tweet' };
  },
});
