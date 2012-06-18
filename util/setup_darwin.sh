unamestr=`uname`
if [[ "$unamestr" == 'Darwin' ]]; then
  ROOT=$(cd $(dirname $0);pwd) 
  /usr/bin/ruby -e "$(/usr/bin/curl -fsSL https://raw.github.com/mxcl/homebrew/master/Library/Contributions/install_homebrew.rb)"
  brew install sdl, sdl_image, sdl_mixer, sdl_ttf
  cabal=`which cabal`
  if [[ "$cabal" == 'cabal not found' ]]; then
    echo 'You must install Haskell Platform for Mac.'
  else
    cabal install sdl
    cabal install sdl-image
    cabal install sdl-mixer
    cabal install sdl-ttf
    cabal configure
    cabal build
    echo 'Set SDL_VIDEODRIVER to X11 as environment variable.'
    echo 'export SDL_VIDEODRIVER=X11'
  fi
else
  echo 'This script must be executed on Mac OS X.'
fi
