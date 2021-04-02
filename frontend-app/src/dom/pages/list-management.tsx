import React, { FC } from 'react';

import * as twitter from '../twitter/models';
import { t } from '../twitter/models';
import * as e from '../effects';
import { RemoteData } from '../utils/remote-data';

type MyDispatch<T> = (_: T) => void;

export type Model = {
  allLists: RemoteData<twitter.List[], Error>;
};

export const init: Model = {
  allLists: { type: 'idle' },
};

export type Msg =
  // fetchLists
  | { type: 'start_fetch_lists' }
  | { type: 'receive_fetch_lists'; lists: twitter.List[] }
  | { type: 'error_fetch_lists'; error: Error };

export const reduce = (init: Model) => (
  model: Model | undefined,
  msg: Msg,
): Model => {
  if (model === undefined) {
    return init;
  }
  switch (msg.type) {
    case 'start_fetch_lists':
    case 'receive_fetch_lists':
    case 'error_fetch_lists': {
      return model;
    }
  }
};

export class Effects {
  constructor(private readonly dispatch: MyDispatch<Msg>) {}

  fetchLists(): Msg {
    e.fetchJson(t('lists/list') + '?reverse=true')
      .then(twitter.parseArray(twitter.parseList))
      .then(
        (lists: twitter.List[]) => {
          this.dispatch({ type: 'receive_fetch_lists', lists });
        },
        (e) => {
          this.dispatch({ type: 'error_fetch_lists', error: e });
        },
      );
    return { type: 'start_fetch_lists' };
  }
}

export type Props = {
  model: Model;
  dispatch: MyDispatch<Msg>;
};

type Com<T = {}> = FC<{ props: Props } & T>;

export const ListManagement: Com = ({ props: { dispatch } }) => {
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
