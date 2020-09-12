TwitAnalysis
============

An experiment-lab for building Twitter tools using the Twitter v1.1 REST API.

- `backend-app/` is a Haskell Stack project, defining the backend server.
- `frontend-app/` is a JavaScript/TypeScript project, building a browser-UI app.

# Dev workflow

Currently not well defined. The critical link is a symlink file:

    $ ls -l backend-app/static
    lrwxr-xr-x  1 eason  staff  22 12 Sep 15:10 backend-app/static -> ../frontend-app/static

which the frontend build-chain writes to, and the backend server serves up.

# License

All rights reserved, easoncxz 2020.

This is until I make up my mind about what license to choose for this one.
