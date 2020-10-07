# twitanalysis

A web server that implements an OAuth 1.0a Consumer against the Twitter v1.1 REST API.

I didn't want to implement much on the server side, but unfortunately, due to the way
OAuth 1.0a works, the Consumer needs to be able to hold some secret keys as secret,
which is not feasible in a purely frontend-app situation. _Some kind of_ backend is
needed, and this project is it.

Main functionality:

- Implement the OAuth 1.0a redirection-based sign-in flow.
- Maintain cookie-sessions.
- Be the Consumer party that signs OAuth 1.0a requests that we send to the
  Twitter v1.1 REST API.
- Serve up the frontend app.

As the outer README already notes, the critical link between the frontend and
backend is the symlink `static/ -> ../frontend-app/static/`.

The frontend app is served purely out of this symlinked directory; there are no
checks in this project to ensure that the files in the static directory is
up-to-date or even present. Some coordination _outside_ of this project needs
to happen, e.g. running the frontend bundling before starting up this server.

## Build and run

    $ brew install haskell-stack
    $ stack build
    $ stack exec twitanalysis-exe

Or more fun during development:

    $ stack ghci
    ...
    ghci> :main

    -- Alternatively:
    ghci> :l app/Main.hs
    ghci> :main

Run tests:

    $ stack test

Or in GHCi:

    ghci> :l test/Spec.hs
    ghci> :main

## Environment

These environment variables are needed by the backend-app server:

### PORT

Used in `src/TwitAnalysis/AppEnv.hs`. This is where the server will listen.

### TWITTER_CONSUMER_KEY, TWITTER_CONSUMER_SECRET

Used in `src/TwitAnalysis/OAuth/AuthFlow.hs`. Register for this on Twitter.
Twitter currently isn't making this easy.
