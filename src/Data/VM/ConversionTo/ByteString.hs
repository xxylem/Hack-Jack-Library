{-# LANGUAGE OverloadedStrings #-}

module Data.VM.ConversionTo.ByteString where

import qualified Data.VM.Model as VM
import qualified Data.Output.Model as OUT

import qualified Data.ByteString.Char8 as BS


convertDirectory :: [VM.File] -> [OUT.OutputFile]
convertDirectory = map convertSingleFile

convertSingleFile :: VM.File -> OUT.OutputFile
convertSingleFile VM.File { VM.program = vmProg
                          , VM.path    = vmPath } =
    OUT.OutputFile { OUT.outputProgram = convertLines vmProg
                   , OUT.path          = vmPath }
        where   convertLines [] = "\n"
                convertLines (l:ls) = convert l <> "\n" <> convertLines ls


class Convert a where
    convert :: a -> BS.ByteString

instance Convert VM.Line where
    convert VM.Line { VM.instruction = i } =
        convert i

instance Convert VM.Instruction where
    convert (VM.AL_VM c) = convert c
    convert (VM.M_VM c)  = convert c
    convert (VM.P_VM c)  = convert c
    convert (VM.F_VM c)  = convert c

instance Convert VM.ArithLogicCommand where
    convert VM.ADD = "add"
    convert VM.SUB = "sub"
    convert VM.NEG = "neg"
    convert VM.EQ_VM = "eq"
    convert VM.GT_VM = "gt"
    convert VM.LT_VM = "lt"
    convert VM.AND = "and"
    convert VM.OR = "or"
    convert VM.NOT = "not"

instance Convert VM.MemoryAccessCommand where
    convert VM.MemCMD { VM.direction    = d
                      , VM.segment      = s
                      , VM.index        = i } =
        convert d <> " " <> convert s <> " " <> BS.pack (show i)

instance Convert VM.Direction where
    convert VM.PUSH = "push"
    convert VM.POP = "pop"

instance Convert VM.Segment where
    convert VM.ARGUMENT = "argument"
    convert VM.LOCAL = "local"
    convert VM.STATIC = "static"
    convert VM.CONSTANT = "constant"
    convert VM.THIS = "this"
    convert VM.THAT = "that"
    convert VM.POINTER = "pointer"
    convert VM.TEMP = "temp"

instance Convert VM.ProgramFlowCommand where
    convert (VM.LABEL l) = "label " <> l
    convert (VM.GOTO l) = "goto " <> l
    convert (VM.IF_GOTO l) = "if-goto " <> l

instance Convert VM.FunctionCommand where
    convert VM.FUN { VM.label = l
                   , VM.nVars = n } =
        "function " <> l <> " " <> BS.pack (show n)
    convert VM.CALL { VM.label = l
                    , VM.nArgs = n } =
        "call " <> l <> " " <> BS.pack (show n)
    convert VM.RETURN = "return"