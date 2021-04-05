import React, { FC } from 'react';

import * as twitter from '../twitter/models';
import { t } from '../twitter/models';
import * as tdb from '../twitter/storage';
import { RemoteData } from '../utils/remote-data';
import * as remoteData from '../utils/remote-data';
import { fetchJson, typecheckNever } from '../utils/utils';

type MyDispatch<T> = (_: T) => T;

export type Model = {
  allLists: RemoteData<twitter.List[], Error>;
};

export const init: Model = {
  allLists: { type: 'idle' },
};

export type Msg = {
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
      return {
        ...model,
        allLists: remoteData.reduce(model.allLists, msg.update),
      };
    }
  }
};

export class Effects {
  constructor(private readonly dispatch: MyDispatch<Msg>) {}

  fetchLists(): Msg {
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
          this.dispatch({
            type: 'fetch_lists',
            update: { type: 'error', error: e },
          });
        },
      )
      .catch((e) => {
        console.error(`tdb.storeLists failed: ${e.message}`, e);
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
}

export type Props = {
  model: Model;
  dispatch: MyDispatch<Msg>;
};

const ListPicker: FC<Props & { effects: Effects }> = ({
  model,
  dispatch,
  effects,
}) => (
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
            <select>
              {model.allLists.data.map((l: twitter.List) => (
                <option key={l.id_str} value={l.slug}>
                  {l.name}
                </option>
              ))}
            </select>
          );
        case 'error':
          return (
            <div className="select">
              An error occurred.
              <code>{JSON.stringify(model.allLists.error, undefined, 4)}</code>
            </div>
          );
        default:
          return typecheckNever(model.allLists);
      }
    })()}
    {(model.allLists.type === 'idle' || model.allLists.type === 'error') && (
      <button type="button" onClick={() => dispatch(effects.fetchLists())}>
        refresh
      </button>
    )}
  </div>
);

export const ListManagement: FC<Props> = ({ model, dispatch }) => {
  const effects = new Effects(dispatch);
  const props = { model, dispatch, effects };
  return (
    <div className="page list-management">
      <p>Manage your Twitter lists</p>
      <div className="sidescroll">
        <div className="source-list">
          <ListPicker {...props} />
          <ul>
            <li>(account in this list)</li>
          </ul>
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
