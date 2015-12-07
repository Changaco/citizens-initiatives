module Settings.StaticFiles where

import Prelude

import Data.ByteString.Lazy (ByteString)
import Data.Default (def)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Language.Haskell.TH (Q, Exp, Name)
import System.Directory (doesFileExist)
import System.Exit (ExitCode(..))
import System.Process (callProcess, readProcessWithExitCode)
import Yesod.Static

import Settings (staticDir)
import Settings.Development

-- | use this to create your static file serving site
staticSite :: IO Static
staticSite = if development then staticDevel staticDir
                            else static      staticDir

-- | This generates easy references to files in the static directory at compile time,
--   giving you compile-time verification that referenced files exist.
--   Warning: any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(staticFiles Settings.staticDir)

combineSettings :: CombineSettings
combineSettings = def
    { csCssPostProcess = minifyWith "clean-css" []
    , csJsPostProcess = minifyWith "uglify-js" ["-mc"]
    }

minifyWith :: String -> [String] -> [FilePath] -> ByteString -> IO ByteString
minifyWith pkg args paths input = do
    let prog = "node_modules/"++pkg++"/bin/"++filter (/='-') pkg
    installed <- doesFileExist prog
    if not installed
        then callProcess "npm" ["install", pkg]
        else return ()
    (code, out, err) <- readProcessWithExitCode prog args (TL.unpack $ TLE.decodeUtf8 input)
    if code /= ExitSuccess
        then error ("Error minifying "++show paths++": "++out++err)
        else return (TLE.encodeUtf8 $ TL.pack out)

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets = combineStylesheets' development combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts = combineScripts' development combineSettings
