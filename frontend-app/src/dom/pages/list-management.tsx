import React, { FC } from 'react';

import * as twitter from '../twitter/models';
import { t } from '../twitter/models';
import * as tdb from '../twitter/storage';
import { RemoteData } from '../utils/remote-data';
import * as remoteData from '../utils/remote-data';
import { fetchJson, typecheckNever, MaybeDefined } from '../utils/utils';

type MyDispatch<T> = (_: T) => T;

export type Model = {
  allLists: RemoteData<twitter.List[], Error>;
  focusedList: MaybeDefined<twitter.List>;
};

export const init: Model = {
  allLists: { type: 'idle' },
  focusedList: undefined,
};

export type Msg =
  | {
      type: 'focus_list';
      focusIdStr: string;
    }
  | {
      type: 'fetch_lists';
      update: RemoteData<twitter.List[], Error>;
    };

export const reduce = (init: Model) => (
  model: Model | undefined,
  msg: Msg,
): Model => {
  if (model === undefined) {
    return init;
  }
  switch (msg.type) {
    case 'fetch_lists': {
      const focusedList = remoteData.toMaybeDefined(msg.update)?.[0];
      console.log(`Focusing on list: ${focusedList?.name}`);
      return {
        ...model,
        allLists: remoteData.reduce(model.allLists, msg.update),
        focusedList,
      };
    }
    case 'focus_list': {
      const focusedList = remoteData.toMaybeDefined(
        remoteData.map(
          (ls: twitter.List[]) =>
            ls.filter((l) => l.id_str === msg.focusIdStr)[0],
        )(model.allLists),
      );
      console.log(`Focusing on list: ${focusedList?.name}`);
      return {
        ...model,
        focusedList,
      };
    }
    default:
      typecheckNever(msg);
      return model;
  }
};

export class Effects {
  constructor(private readonly dispatch: MyDispatch<Msg>) {}

  loadListsFromIdb(): Msg {
    tdb
      .readLists()
      .then((lists: twitter.List[]) => {
        this.dispatch({
          type: 'fetch_lists',
          update: { type: 'ok', data: lists },
        });
      })
      .catch((e) => {
        console.error(`tdb.readLists failed: ${e.message}`, e);
        this.dispatch({
          type: 'fetch_lists',
          update: { type: 'error', error: e },
        });
      });
    return {
      type: 'fetch_lists',
      update: { type: 'loading' },
    };
  }

  fetchListsFromNetwork(opts: {
    silenceNetwork?: boolean;
    silenceIdb?: boolean;
  }): Msg {
    const silenceNetwork = opts.silenceNetwork ?? false;
    const silenceIdb = opts.silenceIdb ?? false;
    fetchJson(t('lists/list') + '?reverse=true')
      .then(twitter.parseArray(twitter.parseList))
      .then(
        (lists: twitter.List[]) => {
          this.dispatch({
            type: 'fetch_lists',
            update: { type: 'ok', data: lists },
          });
          return tdb.storeLists(lists);
        },
        (e) => {
          console.error(`fetchJson failed when loading lists: ${e.message}`);
          if (!silenceNetwork) {
            this.dispatch({
              type: 'fetch_lists',
              update: { type: 'error', error: e },
            });
          }
        },
      )
      .catch((e) => {
        console.error(`tdb.storeLists failed: ${e.message}`, e);
        if (!silenceIdb) {
          this.dispatch({
            type: 'fetch_lists',
            update: { type: 'error', error: e },
          });
        }
      });
    return {
      type: 'fetch_lists',
      update: { type: 'loading' },
    };
  }
}

export type Props = {
  model: Model;
  dispatch: MyDispatch<Msg>;
};

const ListPicker: FC<Props> = ({ model, dispatch }) => {
  const effects = new Effects(dispatch);
  return (
    <div className="list-picker">
      {(() => {
        switch (model.allLists.type) {
          case 'idle':
            return (
              <div className="select">{`Click "refresh" to load lists.`}</div>
            );
          case 'loading':
            return <div className="select">Loading...</div>;
          case 'ok':
            return (
              <select
                value={model.focusedList?.id_str}
                onChange={(e) => {
                  dispatch({ type: 'focus_list', focusIdStr: e.target.value });
                }}
              >
                {model.allLists.data.map((l: twitter.List) => (
                  <option key={l.id_str} value={l.id_str}>
                    {l.name}
                  </option>
                ))}
              </select>
            );
          case 'error':
            return (
              <div className="select">
                An error occurred.
                <code>
                  {JSON.stringify(model.allLists.error, undefined, 4)}
                </code>
              </div>
            );
          default:
            return typecheckNever(model.allLists);
        }
      })()}
      {(model.allLists.type === 'idle' || model.allLists.type === 'error') && (
        <button
          type="button"
          onClick={() => {
            effects.fetchListsFromNetwork({ silenceNetwork: true });
            dispatch(effects.loadListsFromIdb());
          }}
        >
          refresh
        </button>
      )}
    </div>
  );
};

export const ListManagement: FC<Props> = (props) => {
  return (
    <div className="page list-management">
      <p>Manage your Twitter lists</p>
      <div className="sidescroll">
        <div className="source-list">
          <ListPicker {...props} />
          <button type="button">fetch</button>
          <button type="button">save</button>
          <button type="button">load</button>
        </div>
        <div className="focused-account">(Focused account)</div>
        <div className="destination-list">
          Destination list:
          <ul>
            <li>(foo list)</li>
            <li>(bar list)</li>
          </ul>
        </div>
      </div>
    </div>
  );
};
