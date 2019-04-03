{-# LANGUAGE OverloadedStrings #-}

module Data.VM.ConversionTo.ASMCode where

import qualified Data.VM.Model as VM
import qualified Data.Hack.ASM.Model as ASM

import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS (pack)

data InFunctionState =
    Inside VM.Label
  | Outside

data ConversionState = 
    ConversionState { nextEq            :: Integer
                    , nextGt            :: Integer
                    , nextLt            :: Integer
                    , nextReturn        :: Integer
                    , inFunctionState   :: InFunctionState
                    , fileNameNoExt     :: FilePath }

convert :: VM.File -> ASM.File
convert VM.File { VM.program = vmProg
                , VM.path    = vmPath } =
                        undefined

convertMemCMD :: VM.MemoryAccessCommand -> State ConversionState [ASM.Instruction]
convertMemCMD VM.MemCMD { VM.direction = VM.PUSH
                        , VM.segment   = VM.ARGUMENT
                        , VM.index     = vmIndex } =
    return  [   ASM.A vmIndex
            ,   ASM.C ASM.A_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "ARG"
            ,   ASM.C ASM.D_PLUS_M ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_PLUS_1 ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.POP
                        , VM.segment   = VM.ARGUMENT
                        , VM.index     = vmIndex } =
    return  [   ASM.A vmIndex
            ,   ASM.C ASM.A_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "ARG"
            ,   ASM.C ASM.D_PLUS_M ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "R13"
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_MINUS_1 ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "R13"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.PUSH
                        , VM.segment   = VM.LOCAL
                        , VM.index     = vmIndex } =
    return  [   ASM.A vmIndex
            ,   ASM.C ASM.A_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "LCL"
            ,   ASM.C ASM.D_PLUS_M ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_PLUS_1 ASM.M_DEST ASM.NULL_JUMP ]
     
convertMemCMD VM.MemCMD { VM.direction = VM.POP
                        , VM.segment   = VM.LOCAL
                        , VM.index     = vmIndex } =
    return  [   ASM.A vmIndex
            ,   ASM.C ASM.A_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "LCL"
            ,   ASM.C ASM.D_PLUS_M ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "R13"
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_MINUS_1 ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "R13"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP ]
  
convertMemCMD VM.MemCMD { VM.direction = VM.PUSH
                        , VM.segment   = VM.STATIC
                        , VM.index     = vmIndex } = do
    st <- get
    let varName = BS.pack (fileNameNoExt st) <> "." <> BS.pack (show vmIndex)
    return  [   ASM.AL varName
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_PLUS_1 ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.POP
                        , VM.segment   = VM.STATIC
                        , VM.index     = vmIndex } = do
    st <- get
    let varName = BS.pack (fileNameNoExt st) <> "." <> BS.pack (show vmIndex)
    return  [   ASM.AL "SP"
            ,   ASM.C ASM.M_MINUS_1 ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL varName
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.PUSH
                        , VM.segment   = VM.CONSTANT
                        , VM.index     = vmIndex } =
    return  [   ASM.A vmIndex
            ,   ASM.C ASM.A_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_PLUS_1 ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.PUSH
                        , VM.segment   = VM.THIS
                        , VM.index     = vmIndex } =
    return  [   ASM.A vmIndex
            ,   ASM.C ASM.A_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "THIS"
            ,   ASM.C ASM.D_PLUS_M ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_PLUS_1 ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.POP
                        , VM.segment   = VM.THIS
                        , VM.index     = vmIndex } =
    return  [   ASM.A vmIndex
            ,   ASM.C ASM.A_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "THIS"
            ,   ASM.C ASM.D_PLUS_M ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "R13"
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_MINUS_1 ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "R13"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.PUSH
                        , VM.segment   = VM.THAT
                        , VM.index     = vmIndex } =
    return  [   ASM.A vmIndex
            ,   ASM.C ASM.A_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "THAT"
            ,   ASM.C ASM.D_PLUS_M ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_PLUS_1 ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.POP
                        , VM.segment   = VM.THAT
                        , VM.index     = vmIndex } =
    return  [   ASM.A vmIndex
            ,   ASM.C ASM.A_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "THAT"
            ,   ASM.C ASM.D_PLUS_M ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "R13"
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_MINUS_1 ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "R13"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP ]
      
convertMemCMD VM.MemCMD { VM.direction = VM.PUSH
                        , VM.segment   = VM.POINTER
                        , VM.index     = vmIndex } =
    return  [   ASM.A (vmIndex + 3)
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_PLUS_1 ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.POP
                        , VM.segment   = VM.POINTER
                        , VM.index     = vmIndex } =
    return  [   ASM.AL "SP"
            ,   ASM.C ASM.M_MINUS_1 ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.A (vmIndex + 3)
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.PUSH
                        , VM.segment   = VM.TEMP
                        , VM.index     = vmIndex } =
    return  [   ASM.A (vmIndex + 5)
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.AL "SP"
            ,   ASM.C ASM.M_PLUS_1 ASM.M_DEST ASM.NULL_JUMP ]

convertMemCMD VM.MemCMD { VM.direction = VM.POP
                        , VM.segment   = VM.TEMP
                        , VM.index     = vmIndex } =
    return  [   ASM.AL "SP"
            ,   ASM.C ASM.M_MINUS_1 ASM.M_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.A_DEST ASM.NULL_JUMP
            ,   ASM.C ASM.M_COMP ASM.D_DEST ASM.NULL_JUMP
            ,   ASM.A (vmIndex + 5)
            ,   ASM.C ASM.D_COMP ASM.M_DEST ASM.NULL_JUMP ]
