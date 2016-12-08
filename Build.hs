import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions $ do
  "_posts//*.md" *> \_ -> do
    cmd "jekyll build --config=_config/_config.yml"

