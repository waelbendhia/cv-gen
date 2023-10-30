module CV.LuaLatex (runLuaLatex) where

import GHC.IO.Exception
import Relude
import System.Directory
import System.FilePath
import System.Process

runLuaLatex :: FilePath -> String -> ByteString -> IO (ExitCode, Text)
runLuaLatex tmpDir filename source = do
    createDirectoryIfMissing True tmpDir
    let file = joinPath [tmpDir, filename <> ".tex"]
        output = joinPath [tmpDir, filename <> ".pdf"]
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
    renameFile output ("./" <> filename <> ".pdf")
    pure (exit, fromString out)
