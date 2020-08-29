'use strict';

import { getMessage } from './dependencies.js';
import { loadReactComponentOnto } from './react-things.js';

function registerListeners() {
  const helloButton = document.getElementById('one-button');
  const label = document.getElementById('one-label');
  if (helloButton && label) {
    helloButton.addEventListener(
      'click',
      (_mouseEvent) => {
        const t = getMessage();
        label.innerText = t;
      },
    );
  }
  const loadReactButton = document.getElementById('load-react-button');
  const reactContainer = document.getElementById('react-root-load-point');
  if (loadReactButton && reactContainer) {
    loadReactButton.addEventListener(
      'click',
      (_mouseEvent) => {
        loadReactComponentOnto(reactContainer);
      },
    );
  }
}

if (typeof window === 'object' && window.addEventListener) {
  window.addEventListener('load', registerListeners);
}
