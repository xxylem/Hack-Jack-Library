{-# LANGUAGE OverloadedStrings #-}

module Data.Hack.ASM.Model where
  
import Control.Monad.Trans.State
import qualified Data.ByteString.Char8 as BS (ByteString)
import qualified Data.Map.Strict as Map
import System.FilePath (addExtension, dropExtension)

processASMInstructions :: FilePath -> [Instruction] -> File
processASMInstructions fp insts =
    let (noLabels, st) = runState (processLabels insts) initState
        noSymbols      = evalState (resolveAddresses noLabels) st
    in File { program = noSymbols
            , path    = addExtension (dropExtension fp) "asm" }

-- Data model for Hack instructions in ASM format.

data Line = Line { lineNumber :: Integer
                 , instruction :: Instruction }
type Program = [Line]
data File = File { program :: Program
                 , path    :: FilePath }


type AddressVal = Integer
type SymbolTable = Map.Map BS.ByteString AddressVal

initSymbolTable :: SymbolTable
initSymbolTable =
    Map.fromList [  ("SP",      0)
                 ,  ("LCL",     1)
                 ,  ("ARG",     2)
                 ,  ("THIS",    3)
                 ,  ("THAT",    4)
                 ,  ("R0",      0)
                 ,  ("R1",      1)
                 ,  ("R2",      2)
                 ,  ("R3",      3)
                 ,  ("R4",      4)
                 ,  ("R5",      5)
                 ,  ("R6",      6)
                 ,  ("R7",      7)
                 ,  ("R8",      8)
                 ,  ("R9",      9)
                 ,  ("R10",     10)
                 ,  ("R11",     11)
                 ,  ("R12",     12)
                 ,  ("R13",     13)
                 ,  ("R14",     14)
                 ,  ("R15",     15)
                 ,  ("SCREEN",  16384)
                 ,  ("KBD",     24576)
                 ]                 

data ProcessingState = ProcessingState  { nextAddressValue :: Integer 
                                        , nextLineNumber   :: Integer
                                        , symbolTableState :: SymbolTable }
            
initState :: ProcessingState
initState = ProcessingState { nextAddressValue = 16
                            , nextLineNumber   = 0
                            , symbolTableState = initSymbolTable }

incNextLineNumber :: State ProcessingState ()
incNextLineNumber = do
    st <- get
    put $ st { nextLineNumber = 1 + nextLineNumber st }
incNextAddValue :: State ProcessingState ()
incNextAddValue = do
    st <- get
    put $ st { nextAddressValue = 1 + nextAddressValue st }
insertToSymbolTable :: BS.ByteString -> AddressVal -> State ProcessingState ()
insertToSymbolTable newLabel aVal = do
    st <- get
    put $ st { symbolTableState = Map.insert newLabel aVal (symbolTableState st) }

 
processLabels :: [Instruction] -> State ProcessingState [Line]
processLabels [] = return []
processLabels (i:is) = do
    st <- get
    let lineNum = nextLineNumber st
    case i of
      L { label = lb } -> do
                             insertToSymbolTable lb lineNum
                             processLabels is
      _                   -> do
                             incNextLineNumber
                             is' <- processLabels is
                             return $ ( Line { lineNumber = lineNum 
                                             , instruction = i } ) : is'

resolveAddresses :: [Line] -> State ProcessingState [Line]
resolveAddresses [] = return []
resolveAddresses (l:ls) = do
  firstLine <- resolveAddress l
  rest      <- resolveAddresses ls
  return $ firstLine : rest

resolveAddress :: Line -> State ProcessingState Line
resolveAddress l =
    case instruction l of
        S { addressSymbol = sym } -> do
                                     st <- get
                                     let symTab = symbolTableState st
                                     case Map.lookup sym symTab of
                                        Just val -> return $ l { instruction = A val }
                                        Nothing  -> do
                                                    let aVal = nextAddressValue st
                                                    insertToSymbolTable sym aVal
                                                    incNextAddValue
                                                    return $ l { instruction = A aVal }
        _ -> return l       

data Instruction =
    A { address       :: Integer}
  | C { computation   :: Computation
      , destination   :: Destination
      , jump          :: Jump }
  | L { label         :: BS.ByteString}
  | S { addressSymbol :: BS.ByteString }

data Computation =
    ZERO
  | ONE
  | MINUS_ONE
  | D_COMP
  | A_COMP
  | NOT_D
  | NOT_A
  | NEG_D
  | NEG_A
  | D_PLUS_1
  | A_PLUS_1
  | D_MINUS_1
  | A_MINUS_1
  | D_PLUS_A
  | D_MINUS_A
  | A_MINUS_D
  | D_AND_A
  | D_OR_A
  | M_COMP
  | NOT_M
  | NEG_M
  | M_PLUS_1
  | M_MINUS_1
  | D_PLUS_M
  | D_MINUS_M
  | M_MINUS_D
  | D_AND_M
  | D_OR_M

data Destination =
    NULL_DEST
  | M_DEST
  | D_DEST
  | MD
  | A_DEST
  | AM
  | AD
  | AMD

data Jump =
    NULL_JUMP
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP
