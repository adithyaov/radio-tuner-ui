# ReadMe :-)

## Individual Project

### Requirements 

1. cabal-new
2. ghc

### Commands

```
# Directly run
cabal new-run widget

# Install and run
cabal new-install widget
widget # Make sure '/cabal/bin' is in your path.
```

## Using it in your project

1. Add the following under `build-depends` in your cabal file.
  - http-conduit
  - threepenny-gui
  - aeson
  - containers
2. Copy `GUI.hs` and `Models.hs` to your project.
3. Add the modules `GUI` and `Models` under `other-modules` in your cabal file.
4. import the module `GUI` in your `Main` module and add `guiServer` at the end in your `main` function. 
