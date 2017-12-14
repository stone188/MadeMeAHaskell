module MoBettaEngine where

import System.IO

import qualified Data.HashMap as HM
import Control.Monad.State
import Control.Applicative
import Data.Maybe (fromMaybe)

import MoBettaAST

type Env = HM.Map String Integer


emptyEnv :: Env
emptyEnv = HM.fromList []


type Computation t = StateT Env IO t
type Action = Computation ()
type IntCalc = Computation Integer
type BoolCalc = Computation Bool


statementAction :: Statement -> Action
statementAction (Print e) = printAction (intCalc e)
statementAction (Msg s) = msgAction s
statementAction (Read v) =  readAction v
statementAction (If b s1 s2) =
  ifAction (boolCalc b) (statementAction s1) (statementAction s2)
                        
statementAction (While b s) = whileAction (boolCalc b) (statementAction s)
                        
statementAction (Assign v e) = assignAction v (intCalc e)
                        
statementAction (Block ls) = blockAction $ map statementAction ls
                        

makeProgram ls = blockAction $ map statementAction ls

doIO :: IO a -> Computation a
doIO = lift

updateEnv :: String -> Integer -> Computation ()
updateEnv name val = modify $ HM.insert name val

retrieveEnv :: String -> Computation Integer
retrieveEnv name = do
  val <- gets $ HM.lookup name
  return $ fromMaybe (varNotFound name) val
  where
    varNotFound name = error $ "Identifier \"" ++ name ++ "\" not defined."

readAction :: String -> Action
readAction v = do
  x <- doIO getInt
  updateEnv v x
  where
    getInt = do
      inp <- getLine
      return $ read inp


msgAction :: String -> Action
msgAction s = doIO $ putStr s


printAction :: IntCalc -> Action
printAction intCalc = do
  n <- intCalc 
  doIO $ putStr $ show n 


assignAction :: String -> IntCalc -> Action
assignAction v intCalc = do
  n <- intCalc 
  updateEnv v n 


ifAction :: BoolCalc -> Action -> Action -> Action
ifAction boolCalc action1 action2 = do
  cond <- boolCalc 
  if cond then     
    action1
  else
    action2

whileAction :: BoolCalc -> Action -> Action
whileAction boolCalc action = do
  cond <- boolCalc 
  when cond loop   
  where
    loop = do
      action
      whileAction boolCalc action

blockAction :: [Action] -> Action
blockAction [] = return ()
blockAction (a:ls) = do
  a
  blockAction ls

aBinOps =
  [ (Add, (+))
  , (Sub, (-))
  , (Mul, (*))
  , (Div, div)
  , (Mod, mod)]

aUnOps =  [(Neg, negate)]

bBinOps =
  [ (And, (&&))
  , (Or, (||))]

bUnOps =  [(Not, not)]

relnOps =
  [ (Greater, (>))
  , (GreaterEqual, (>=))
  , (Less, (<))
  , (LessEqual, (<=))
  , (Equal, (==))
  , (NEqual, (/=))]

opLookup :: Eq const => const -> [(const, sem)] -> sem
opLookup op opTable =
  fromMaybe (error "Undefined operator. Should never happen.")
            (lookup op opTable)

boolCalc :: BExpr -> BoolCalc
boolCalc (BoolConst b) = return b
boolCalc (Reln cOp expr1 expr2)
  = liftA2 (opLookup cOp relnOps) (intCalc expr1) (intCalc expr2)
boolCalc (BBin op expr1 expr2)
  = liftA2 (opLookup op bBinOps) (boolCalc expr1) (boolCalc expr2)
boolCalc (BUn op expr)
  = fmap (opLookup op bUnOps) (boolCalc expr)

intCalc :: AExpr -> IntCalc
intCalc (Var v) = retrieveEnv v
intCalc (IntConst val) = return val
intCalc (ABin op expr1 expr2)
  = liftA2 (opLookup op aBinOps) (intCalc expr1) (intCalc expr2)
intCalc (AUn op expr)
  = fmap (opLookup op aUnOps) (intCalc expr)
