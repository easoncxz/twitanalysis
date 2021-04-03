import React from 'react';

import { Status } from '../twitter/models';
import * as effects from '../effects';
import * as core from '../core';

export const View: React.FC<{
  model: core.Model;
  dispatch: (_: core.Msg) => void;
  effects: effects.Effects;
}> = ({ model, dispatch, effects }) => {
  const go = () => {
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
        <button onClick={go} disabled={model.fetchingFaves}>
          Fetch from Twitter
        </button>
        <button onClick={() => dispatch(effects.readAllTweets())}>
          Load from IndexedDB
        </button>
        <button onClick={() => dispatch(effects.putTweets(model.faves))}>
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
