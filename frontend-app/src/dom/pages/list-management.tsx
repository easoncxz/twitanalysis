import React, { FC } from 'react';

import * as twitter from '../twitter/models';
import { t } from '../twitter/models';
import { RemoteData } from '../utils/remote-data';
import * as remoteData from '../utils/remote-data';
import { fetchJson } from '../utils/utils';

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
    fetchJson(t('lists/list') + '?reverse=true')
      .then(twitter.parseArray(twitter.parseList))
      .then((lists: twitter.List[]) => {
        this.dispatch({
          type: 'fetch_lists',
          update: { type: 'ok', data: lists },
        });
      })
      .catch((e) => {
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

export const ListManagement: FC<Props> = ({ dispatch }) => {
  const effects = new Effects(dispatch);
  const ListPicker = () => (
    <div className="list-picker">
      <select>
        <option value="foo">foo</option>
        <option value="bar">bar</option>
      </select>
      <button type="button" onClick={() => dispatch(effects.fetchLists())}>
        refresh
      </button>
    </div>
  );

  return (
    <div className="page list-management">
      <p>Manage your Twitter lists</p>
      <div className="sidescroll">
        <div className="source-list">
          <ListPicker />
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
