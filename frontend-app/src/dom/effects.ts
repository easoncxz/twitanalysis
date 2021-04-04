import type { Dispatch } from 'redux';

import * as core from './core';
import { Status, t, parseStatus } from './twitter/models';
import { fetchJson } from './utils/utils';

export class Effects {
  constructor(private readonly dispatch: Dispatch<core.Msg>) {}

  noop(): core.Msg {
    return {
      type: 'noop',
    };
  }

  sendTweet(text: string): core.Msg {
    const body = new URLSearchParams();
    body.set('status', text);
    fetchJson(t('statuses/update'), {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
      body,
    })
      .then(parseStatus)
      .then(
        (status: Status) => {
          this.dispatch({ type: 'receive_send_tweet', status });
        },
        (e) => this.dispatch({ type: 'error_send_tweet', error: e }),
      );
    return { type: 'start_send_tweet' };
  }
}
