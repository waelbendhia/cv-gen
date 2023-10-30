module CV.LuaLatex (runLuaLatex) where

import GHC.IO.Exception
import Relude
import System.Directory
import System.FilePath
import System.Process

runLuaLatex :: FilePath -> String -> ByteString -> IO (ExitCode, Text)
runLuaLatex tmpDir filepath source = do
    createDirectoryIfMissing True tmpDir
    let filename = takeFileName filepath
        file = tmpDir </> filename -<.> "tex"
        output = tmpDir </> filename -<.> "pdf"
    writeFileBS file source
    let args =
            [ "-halt-on-error"
            , "-interaction"
            , "nonstopmode"
            , "-output-directory"
            , tmpDir
            , file
            ]
    (exit, out, _) <- liftIO $ readProcessWithExitCode "lualatex" args ""
    print (exit, out)
    renameFile output ("./" </> filename -<.> "pdf")
    pure (exit, fromString out)
