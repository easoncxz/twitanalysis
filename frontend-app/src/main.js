'use strict';

import { getMessage } from './dependencies.js';
import { loadReactComponentOnto } from './react-things.js';

function registerListeners() {
  const helloButton = document.getElementById('one-button');
  const label = document.getElementById('one-label');
  helloButton.addEventListener(
    'click',
    (_mouseEvent) => {
      label.innerText = getMessage();
    },
  );
  const loadReactButton = document.getElementById('load-react-button');
  const reactContainer = document.getElementById('react-root-load-point');
  loadReactButton.addEventListener(
    'click',
    (_mouseEvent) => {
      loadReactComponentOnto(reactContainer);
    },
  );
}

window.addEventListener('load', registerListeners);
