module Main where

import qualified LLVM.AST                      as AST                     

import           Data.String.Conversions
import           Data.Text                      ( Text )
import qualified LLVM.Module                   as LLVM
import           LLVM.Context
import           LLVM.Analysis

import qualified LLVM.IRBuilder.Module         as L
import qualified LLVM.IRBuilder.Monad          as L
import qualified LLVM.IRBuilder.Instruction    as L
import qualified LLVM.IRBuilder.Constant       as L

import           System.Directory
import           System.Process
import           System.Posix.Temp

import           Control.Exception

fixnum :: Integer -> Codegen Operand
fixnum num = L.ret =<< pure $ L.int32 $ fromIntegral num

compile :: Module -> FilePath -> IO ()
compile llvmModule outfile = do
  bracket (mkdtemp "build") removePathForcibly $ \buildDir ->
    withCurrentDirectory buildDir $ do
      let llvm = "output.ll"
          runtime = "runtime.c"
      withContext $ \ctx -> LLVM.withModuleFromAST
        cts
        llvmModule
        (\modl -> verify modl >> LLVM.writeBitcodeToFile (LLVM.File llvm) modl)
      callProcess
        "clang"
        ["-Wno-override-module", "-lm", llvm, runtime, "-o", outfile]             


main :: IO ()
main = do 
  x <- getLine
  modl <- fixnum (read x)
  compile modl "test"
