import React from 'react';

import { Status, t } from '../twitter/models';
import * as twitter from '../twitter/models';
import * as tdb from '../twitter/storage';
import * as core from '../core';
import { fetchJson } from '../utils/utils';

export class Effects {
  constructor(private readonly dispatch: (_: core.Msg) => void) {}

  fetchFaves(
    nik: string,
    {
      count = 200,
      since_id,
      max_id,
    }: {
      count?: number;
      max_id?: string;
      since_id?: string;
    },
  ): core.Msg {
    const searchParams = new URLSearchParams();
    searchParams.append('screen_name', nik);
    searchParams.append('count', count.toString());
    if (since_id !== undefined) {
      searchParams.append('since_id', since_id);
    }
    if (max_id !== undefined) {
      searchParams.append('max_id', max_id);
    }
    fetchJson(t('favorites/list') + '?' + searchParams.toString())
      .then(twitter.parseArray(twitter.parseStatus))
      .then(
        (statuses: Status[]) => {
          this.dispatch({ type: 'receive_fetch_faves', statuses });
        },
        (e) => this.dispatch({ type: 'error_fetch_faves', error: e }),
      );
    return {
      type: 'start_fetch_faves',
    };
  }
}

export const View: React.FC<{
  model: core.Model;
  dispatch: (_: core.Msg) => void;
}> = ({ model, dispatch }) => {
  const effects = new Effects(dispatch);
  const fetchFromNetwork = () => {
    const nick =
      model.user.type === 'ok' ? model.user.data.screen_name : model.faveNick;
    dispatch(
      effects.fetchFaves(nick, {
        count: 200,
        max_id: model.faves.length
          ? model.faves[model.faves.length - 1].id_str
          : undefined,
      }),
    );
  };
  const faveList = model.faves
    .filter((f) => new RegExp(model.searchFaves ?? '.').exec(f.text))
    .map((f: Status, i: number) => <li key={'fave-' + String(i)}>{f.text}</li>);
  return (
    <>
      <div>
        <span>Fetch your favourited tweets</span>
        <input
          type="text"
          value={
            model.user.type === 'ok'
              ? model.user.data.screen_name
              : model.faveNick
          }
          onChange={(e) =>
            dispatch({ type: 'update_fave_nick', nick: e.target.value })
          }
        />
        <button onClick={fetchFromNetwork} disabled={model.fetchingFaves}>
          Fetch from Twitter
        </button>
        <button
          onClick={() => {
            tdb.readAllTweets().then((ss: Status[]) => {
              void ss;
            });
          }}
        >
          Load from IndexedDB
        </button>
        <button
          onClick={() => {
            tdb.putTweets(model.faves).then(() => {
              void 3;
            });
          }}
        >
          Save to IndexedDB
        </button>
      </div>
      <div>
        <span>Search:</span>
        <input
          type="text"
          value={model.searchFaves ?? ''}
          onChange={(e) =>
            dispatch({
              type: 'update_search_faves',
              search: e.target.value,
            })
          }
        />
      </div>
      {faveList.length > 0 ? <ul>{faveList}</ul> : <p>No faves</p>}
    </>
  );
};
