import React from 'react';

import { User, Status, t } from '../twitter/models';
import * as twitter from '../twitter/models';
import * as tdb from '../twitter/storage';
import { fetchJson, typecheckNever } from '../utils/utils';
import { RemoteData } from '../utils/remote-data';
import * as remoteData from '../utils/remote-data';

export type Model = {
  faves: Status[];
  networkFaves: RemoteData<Status[], Error>;
  faveNick: string;
  searchFaves?: string;
};

export const init: Model = {
  faves: [],
  networkFaves: { type: 'idle' },
  faveNick: "(somebody's Twitter ID)",
  searchFaves: undefined,
};

export type Msg =
  | {
      type: 'network_faves';
      update: RemoteData<Status[], Error>;
    }
  | { type: 'update_fave_nick'; nick: string }
  | { type: 'update_search_faves'; search: string }
  | { type: 'receive_faves_from_idb'; statuses: Status[] };

export const reduce = (user: RemoteData<User, Error>) => (init: Model) => (
  model: Model | undefined,
  msg: Msg,
): Model => {
  if (model === undefined) {
    return init;
  }
  switch (msg.type) {
    case 'network_faves': {
      return {
        ...model,
        networkFaves: remoteData.reduce(model.networkFaves, msg.update),
      };
    }
    case 'update_fave_nick': {
      if (user.type === 'ok' || user.type === 'loading') {
        // Ignore the upate; we will use the nick of the current user.
        return { ...model };
      }
      return {
        ...model,
        faveNick: msg.nick,
      };
    }
    case 'update_search_faves':
      return {
        ...model,
        searchFaves: msg.search,
      };
    case 'receive_faves_from_idb':
      return {
        ...model,
        faves: msg.statuses,
      };

    default:
      typecheckNever(msg);
      return model;
  }
};

export class Effects {
  constructor(private readonly dispatch: (_: Msg) => void) {}

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
  ): Msg {
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
      .then(async (statuses: Status[]) => {
        this.dispatch({
          type: 'network_faves',
          update: { type: 'ok', data: statuses },
        });
        await tdb.putTweets(statuses);
        const ss = await tdb.readAllTweets();
        this.dispatch({
          type: 'receive_faves_from_idb',
          statuses: ss,
        });
      })
      .catch((e) =>
        this.dispatch({
          type: 'network_faves',
          update: {
            type: 'error',
            error: e,
          },
        }),
      );
    return {
      type: 'network_faves',
      update: { type: 'loading' },
    };
  }
}

export const View: React.FC<{
  user: RemoteData<User, Error>;
  model: Model;
  dispatch: (_: Msg) => void;
}> = ({ user, model, dispatch }) => {
  const effects = new Effects(dispatch);
  const fetchFromNetwork = () => {
    const nick = user.type === 'ok' ? user.data.screen_name : model.faveNick;
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
          value={user.type === 'ok' ? user.data.screen_name : model.faveNick}
          onChange={(e) =>
            dispatch({ type: 'update_fave_nick', nick: e.target.value })
          }
        />
        <button
          onClick={fetchFromNetwork}
          disabled={model.networkFaves.type === 'loading'}
        >
          Fetch from Twitter
        </button>
        <button
          onClick={() => {
            console.log(`Reading faves from idb...`);
            tdb.readAllTweets().then((statuses: Status[]) => {
              dispatch({ type: 'receive_faves_from_idb', statuses });
            });
          }}
        >
          Load from IndexedDB
        </button>
        <button
          onClick={() => {
            tdb.putTweets(model.faves);
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
