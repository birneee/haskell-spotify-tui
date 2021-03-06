# Haskell Spotify TUI <br/> FFP-Project

## Description

Terminal user interface to control Spotify music playback.
Created as an examination performance for the module Fortgeschrittene Funktionale Programmierung at the Ludwig-Maximilians-Universität München.

![TUI Screenshot](/screenshots/tui.png "TUI Screenshot")

## Authors

- Benedikt Spies
- Kai-Chun Lin
- Yang Mao

## Requirements

- Stack installation
- Spotify Account

> :warning: App is only tested in Gnome Terminal with a Spotify Premium Account

## Setup

1. Create a Spotify App (https://developer.spotify.com/dashboard)

- go to Edit Settings and add Redirect URI: http://localhost:8888/callback
- copy clientId and clientSecret to `config.json` file
  - `config.json` is created on first run in current directory

2. Run application and login to Spotify (browser should open automatically)

Example config.json:

```json
{
  "clientId": "<clientId>",
  "clientSecret": "<clientSecret>"
}
```

## Run

```sh
stack run
```

## Build and Run Executable

```sh
stack build
.stack-work/dist/*/*/build/haskell-spotify-tui/haskell-spotify-tui
```

## Install and Run on System

```sh
stack install
haskell-spotify-tui
```

## Contributing

- New commits should be `-Wall` clean
- Committed code should be formatted with `ormolu`

## Used Libraries

- [aeson](https://hackage.haskell.org/package/aeson)
- [http-client](https://hackage.haskell.org/package/http-client)
- [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels)
- [lens](https://hackage.haskell.org/package/lens)
- [monad-par](https://hackage.haskell.org/package/monad-par)
- [template-haskell](https://hackage.haskell.org/package/template-haskell)
- [transformers](https://hackage.haskell.org/package/transformers)
- [warp](https://hackage.haskell.org/package/warp)
- [brick](https://hackage.haskell.org/package/brick) for terminal user interface (TUI)
