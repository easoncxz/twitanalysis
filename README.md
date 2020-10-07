TwitAnalysis
============

An experiment-lab for building Twitter tools using the Twitter v1.1 REST API.

- `backend-app/` is a Haskell Stack project, defining the backend server.
- `frontend-app/` is a JavaScript/TypeScript project, building a browser-UI app.
- `docs/` is the Jekyll site that serves the [Github Pages for this repo][gh-pages]

[gh-pages]: https://easoncxz.github.io/twitanalysis

# Dev workflow

Currently not well defined. The critical link is a symlink file:

    $ ls -l backend-app/static
    lrwxr-xr-x  1 eason  staff  22 12 Sep 15:10 backend-app/static -> ../frontend-app/static

which the frontend build-chain writes to, and the backend server serves up. 

## Environment

The environment variables needed for the backend-app server during runtime is listed in `backend-app/README.md`.

The following environment variables are needed for the build/test/package/publish process (mostly running Rake commands):

### GITHUB_OAUTH_TOKEN

Obtain this token as per these docs:

- Old doc: https://developer.github.com/apps/building-oauth-apps/authorizing-oauth-apps/
- New doc: https://docs.github.com/en/free-pro-team@latest/developers/apps/authorizing-oauth-apps

## Build

There is a build system piggy-backing on the Jekyll Gem bundle (`Gemfile`) using 
Ruby Rake:

```
$ ./activate.sh
(.gems) $ bundle exec rake  # build everything
```

I may add deploy commands as `rake` targets; why not, it's simple to read, 
write, and run.

## Publish

```
(.gems) $ bundle exec rake 'publish[some-git-tag]'
```

The Git tag doesn't have to exist; it will be created if it doesn't exist.

This Rake task is idempotent; it's safe to run it again on the same args.

# License

All rights reserved, easoncxz 2020.

This is until I make up my mind about what license to choose for this one.
