import qualified Data.Set as S

import Initialize
import TitleScene
import GameManager

main = do
  withGameInit mainLoop

mainLoop window = do
  putStrLn "GameInitialized"
  runGame S.empty [Object titleScene]
