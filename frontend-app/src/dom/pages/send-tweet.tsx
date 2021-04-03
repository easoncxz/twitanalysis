import React from 'react';

import * as core from '../core';
import * as effects from '../effects';

export const View: React.FC<{
  model: core.Model;
  dispatch: (_: core.Msg) => void;
  effects: effects.Effects;
}> = ({ model, dispatch, effects }) => {
  return (
    <div>
      <h2>Send tweet</h2>
      <form action="">
        <textarea
          value={model.pendingTweet}
          onChange={(e) => {
            dispatch({
              type: 'update_pending_tweet',
              text: e.target.value,
            });
          }}
          disabled={model.sendingTweet}
        ></textarea>
        <br />
        <pre>{model.pendingTweet}</pre>
        <input
          type="submit"
          onClick={(e) => {
            e.preventDefault();
            dispatch(effects.sendTweet(model.pendingTweet));
          }}
          disabled={model.sendingTweet}
          value="Send this tweet"
        />
      </form>
      <p>Sent tweets:</p>
      <ul>
        {model.sentTweets.map((st, i) => (
          <li key={`sentTweets-${i}`}>
            <code>{st.created_at}</code> - {st.text}
          </li>
        ))}
      </ul>
    </div>
  );
};
