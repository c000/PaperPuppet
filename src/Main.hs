import Graphics.UI.SDL (Surface)

import Initialize
import TitleScene
import GameManager
import Class.GameScene

main :: IO ()
main = do
  withGameInit main'

main' :: Surface -> IO ()
main' w = do
  putStrLn "GameInitialized"
  gv <- defaultGV w
  let title = titleScene
  start gv title
  runGame gv [SceneObject title]
