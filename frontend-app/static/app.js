"use strict";

import {
  getMessage,
} from "./dependencies.js";

function registerListeners() {
  const button = document.getElementById("one-button");
  const label = document.getElementById("one-label");
  button.addEventListener(
    "click",
    (_mouseEvent) => {
      label.innerText = getMessage();
    },
  );
}

window.addEventListener("load", registerListeners);
