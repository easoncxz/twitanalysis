import React, { FC } from 'react';
import type * as Redux from 'redux';

import * as core from './core';
import { Effects } from './effects';

type Props = {
  model: core.Model;
  dispatch: Redux.Dispatch<core.Msg>;
  effects: Effects;
};

export const ListManagement: FC<{ props: Props }> = () => {
  return (
    <div className="page list-management">
      <p>Manage your Twitter lists</p>
      <div className="sidescroll">
        <div className="source-list">
          <code>ListPicker</code>
          <p>(List name)</p>
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
