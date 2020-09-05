# Haskell Spotify TUI <br/> FFP-Project 

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
- [template-haskell](https://hackage.haskell.org/package/template-haskell)
- [transformers](https://hackage.haskell.org/package/transformers)
- [warp](https://hackage.haskell.org/package/warp)
- [brick](https://hackage.haskell.org/package/brick) for terminal user interface (TUI)
- ... TODO