import Graphics.UI.SDL (Surface)

import Initialize
import TitleScene
import GameManager

main :: IO ()
main = do
  withGameInit main'

main' :: Surface -> IO ()
main' w = do
  putStrLn "GameInitialized"
  runGame (defaultGV w) [Object titleScene]
