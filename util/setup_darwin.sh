unamestr=`uname`
if [[ "$unamestr" == 'Darwin' ]]; then
  ROOT=$(cd $(dirname $0);pwd) 
  /usr/bin/ruby -e "$(/usr/bin/curl -fsSL https://raw.github.com/mxcl/homebrew/master/Library/Contributions/install_homebrew.rb)"
  brew install sdl, sdl_image, sdl_mixer, sdl_ttf
  git clone git@github.com:c255/PaperPuppet.git $ROOT
  cd $ROOT/PaperPuppet
  cabal=`which cabal`
  if [[ "$cabal" == 'cabal not found' ]]; then
    cabal install sdl, sdl-image, sdl-mixer, sdl-ttf
    cabal configure
    cabal build
    echo 'Set SDL_VIDEODRIVER to X11 as environment variable.'
    echo 'extern SDL_VIDEODRIVER=X11'
  else
    echo 'You must install Haskell Platform for Mac.'
  fi
else
  echo 'This script must be executed on Mac OS X.'
fi
