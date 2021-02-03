module Compilator where

import Lexer
import Parser
import Translator
import Memory
import ArrayOptimalization
import Debug

import qualified Data.Map as Map
import qualified Data.List as List
import Data.List

import ConstantsFolding
import Register
import Assigments
import Debug.Trace

mem :: MemoryStructure
mem = getMemoryAllocation [SingleDeclaration "i", SingleDeclaration "i'", SingleDeclaration "j", SingleDeclaration "n", ArrayDeclaration "sito" 2 100, SingleDeclaration "-io"]

test1 :: RegisterState
test1 = Map.fromList [(A, [Constant 10]), (B, [Variable (SingleVar "i") False]), (C, [Variable (ArrayVar "array" "i") False]), (D, []), (E, []), (F, [])]

oppositeCond :: Condition -> Condition
oppositeCond (Cond v1 op v2) = Cond v1 (oppositeComperator op) v2

optimizeExp :: Identifier -> Expression -> Expression
optimizeExp _ (Add (Const c) v) = Add v (Const c)
optimizeExp new_id (Add (Var id) (Var id'))
    | id == id' = Mul (Var id) (Const 2)
    | new_id == id' = Add (Var id') (Var id)
    | otherwise = Add (Var id) (Var id')
optimizeExp _ (Mul (Const c) v) = Mul v (Const c)
optimizeExp new_id (Mul (Var id) (Var id'))
    | new_id == id' = Mul (Var id') (Var id)
    | otherwise = Mul (Var id) (Var id')
optimizeExp _ exp = exp

optimizeCond :: Condition -> Condition
optimizeCond (Cond (Const 0) Translator.EQ v) = Cond v Translator.EQ (Const 0)
optimizeCond (Cond (Const 0) Translator.NE v) = Cond v Translator.NE (Const 0)
optimizeCond (Cond (Const 0) Translator.LT v) = Cond v Translator.NE (Const 0)
optimizeCond (Cond (Const 1) Translator.LE v) = Cond v Translator.NE (Const 0)
optimizeCond (Cond (Const 1) Translator.GT v) = Cond v Translator.EQ (Const 0)
optimizeCond (Cond v Translator.GT (Const 0)) = Cond v Translator.NE (Const 0)
optimizeCond (Cond v Translator.GE (Const 1)) = Cond v Translator.NE (Const 0)
optimizeCond (Cond v Translator.LT (Const 1)) = Cond v Translator.EQ (Const 0)
optimizeCond cond = cond

fake1 = Var (SingleVar "-fake1")
fake2 = Var (SingleVar "-fake2")
fake3 = Var (SingleVar "-fake3")

optimalLoopRegistersState :: MemoryStructure -> RegisterState -> Condition -> [Command] -> RegisterState
optimalLoopRegistersState memory prev cond ins = case cond of 
    Cond v _ (Const 0) -> caped_state
        where
            (test_state, _) = translate memory clearState [] ins
            test_regs = getRegisters test_state [v] [] ins
            test_reg1 = test_regs Map.! v
            (test_load_state, _) = loadUnsavedValues memory test_state [(test_reg1, v)] ins
            (flatten_state, _) = flattenRegistersState memory test_load_state ins
            caped_state = capContent 5 ins flatten_state
    Cond v1 _ v2 -> caped_state
        where
            (test_state, _) = translate memory clearState [] ins
            test_regs = getRegisters test_state [v1, v2, fake1] [] ins
            test_reg1 = test_regs Map.! v1
            test_reg2 = test_regs Map.! v2
            test_regTmp = test_regs Map.! fake1
            (test_load_state, _) = loadUnsavedValues memory test_state [(test_reg1, v1), (test_reg2, v2), (test_regTmp, Const 0)] ins
            (flatten_state, _) = flattenRegistersState memory (Map.insert test_regTmp [] test_load_state) ins
            caped_state = capContent 5 ins flatten_state

optimalFirstCondLoopRegistersState :: MemoryStructure -> RegisterState -> Condition -> [Command] -> RegisterState
optimalFirstCondLoopRegistersState memory prev cond ins = case cond of 
    Cond v _ (Const 0) -> caped_state
        where            
            test_regs = getRegisters clearState [v] [] ins
            test_reg1 = test_regs Map.! v
            (test_load_state, _) = loadUnsavedValues memory clearState [(test_reg1, v)] ins
            (loop_state, _) = translate memory test_load_state [] ins
            (flatten_state, _) = flattenRegistersState memory loop_state ins
            caped_state = capContent 5 ins flatten_state
    Cond v1 _ v2 -> caped_state
        where
            test_regs = getRegisters clearState [v1, v2, fake1] [] ins
            test_reg1 = test_regs Map.! v1
            test_reg2 = test_regs Map.! v2
            test_regTmp = test_regs Map.! fake1
            (test_load_state, _) = loadUnsavedValues memory clearState [(test_reg1, v1), (test_reg2, v2), (test_regTmp, Const 0)] ins
            (test_loop_state, _) = translate memory (Map.insert test_regTmp [] test_load_state) [] ins
            (flatten_state, _) = flattenRegistersState memory test_loop_state ins
            caped_state = capContent 5 ins flatten_state


fillArray :: MemoryStructure -> RegisterState -> [Command] -> Integer -> Integer -> String -> Integer -> (RegisterState, [Output])
fillArray memory state futureCommands from to name val = (state', load_code++loop_code)
    where
        address = memory Map.! name
        regs =  getRegisters state [Const (from+address), Const (Prelude.max 0 (to-from+1)), Const val] [] futureCommands
        iter = regs Map.! Const (Prelude.max 0 (to-from+1))
        pointer = regs Map.! Const (from+address)
        toSave = regs Map.! Const val
        (load_state, load_code) = savePrevAndLoadValue memory state [(pointer, Const (from+address)), (iter, Const (Prelude.max 0 (to-from+1))), (toSave, Const val)] futureCommands
        loop_code = [JZERO iter 5, STORE toSave pointer, INC pointer, DEC iter, JUMP (-4)]
        state' = Map.insert pointer [Constant to] $ Map.insert iter [Constant 0] load_state
        arraySet = (\v -> Variable (ArrayConst name v) True) <$> [from..to]

compile :: MemoryStructure -> [Command] -> [Output]
compile memory comands = snd (translate memory clearState [] comands) ++ [HALT]

translate' :: MemoryStructure -> RegisterState -> [[Output]] -> [Command] -> (RegisterState, [Output])
translate' m s acc (command:rest) = translate m s ([COMMENT (showRegs s++" - "++show command)]:acc) (command:rest)
translate' m s acc [] = translate m s acc []

translate :: MemoryStructure -> RegisterState -> [[Output]] -> [Command] -> (RegisterState, [Output])
translate _ state acc [] = (state, concat $ reverse acc)
--READ
translate memory state acc commands@(Read id:rest) = translate' memory state' ([GET reg]:code:acc) rest
    where
        (prev_state_array_saved, code_array_saved) = saveDependingArrays id memory commands state
        state_array_saved = notifySavedVariable id prev_state_array_saved
        regs = getRegisters state_array_saved [] [fake1] commands
        reg = regs Map.! fake1
        (load_state, code) = loadAddressToRegister memory state_array_saved id reg commands
        state' = removeDependingContent id load_state
--WRITE
translate memory state acc commands@(Write val:rest) = case val of
    Const const -> translate' memory state' ([STORE regVal regAddress, PUT regAddress]:code:acc) rest
        where
            ioAddress = memory Map.! "-io"
            regs = getRegisters state [Const ioAddress, Const const] [] rest
            regAddress = regs Map.! Const ioAddress
            regVal = regs Map.! Const const
            (state', code) = savePrevAndLoadValue memory state [(regAddress, Const ioAddress), (regVal, Const const)] rest
    Var id -> translate' memory state' ((save_code++code++[PUT regAddress]):acc) rest
        where
            regs = getRegisters state [] [fake1] rest
            regAddress = regs Map.! fake1 
            (save_state, save_code) = case registersToSaveInOrderToWrite state id of
                [] -> (state, [])
                regs_to_save -> saveRegistersState memory state regs_to_save rest
            (state', code) = loadAddressToRegister memory save_state id regAddress commands 
-- FOR TO
translate memory state acc (ForUp ('*':num) (Const c1) (Const c2) ins:rest) = 
    translate memory state acc ([Assign iter (SingleValue (Const (Prelude.max (c2 - c1 + 1) 0))), loop] ++ rest)
    where
        iter = SingleVar (name++"'")
        name = "-loop"++num
        loop = While (Cond (Var iter) Translator.NE (Const 0)) (Assign iter (Sub (Var iter) (Const 1)):ins)
translate memory state acc (ForUp ('*':num) v1 v2 ins:rest) = 
    translate memory state acc ([Assign iter (Add v2 (Const 1)), Assign iter (Sub (Var iter) v1), loop] ++ rest)
    where
        iter = SingleVar (name++"'")
        name = "-loop"++num
        loop = While (Cond (Var iter) Translator.NE (Const 0)) (Assign iter (Sub (Var iter) (Const 1)):ins)
translate memory state acc ((ForUp n (Const from) (Const to) [Assign (ArrayVar array n') (SingleValue (Const val))]):rest)
    | n == n' = translate' memory state' (code:acc) rest
    | otherwise = translate memory state acc (Assign (ArrayVar array n') (SingleValue (Const val)):rest)
    where
        (state', code) = fillArray memory state rest from to array val
translate memory state acc ((ForUp name v1 v2 ins@(_:_:_:_:_:_)):rest) = 
    translate memory state acc ([Assign pointer (SingleValue v1), Assign limit (SingleValue v2), loop] ++ rest)
    where
        pointer =  SingleVar name
        limit = SingleVar (name++"'")
        loop = While (Cond (Var pointer) Translator.LE (Var limit)) (ins++[Assign pointer (Add (Var pointer) (Const 1))])
translate memory state acc (ForUp name (Const c1) (Const c2) ins:rest) = 
    translate memory state acc ([Assign iter (SingleValue (Const (Prelude.max (c2 - c1 + 1) 0))), Assign pointer (SingleValue (Const c1)), loop] ++ rest)
    where
        pointer =  SingleVar name
        iter = SingleVar (name++"'")
        loop = While (Cond (Var iter) Translator.NE (Const 0)) (ins++[Assign pointer (Add (Var pointer) (Const 1)), Assign iter (Sub (Var iter) (Const 1))])
translate memory state acc (ForUp name v1 v2 ins:rest) = 
    translate memory state acc ([Assign iter (Add v2 (Const 1)), Assign iter (Sub (Var iter) v1), Assign pointer (SingleValue v1), loop] ++ rest)
    where
        pointer =  SingleVar name
        iter = SingleVar (name++"'")
        loop = While (Cond (Var iter) Translator.NE (Const 0)) (ins++[Assign pointer (Add (Var pointer) (Const 1)), Assign iter (Sub (Var iter) (Const 1))])
--FOR DOWNTO
translate memory state acc (ForDown ('*':num) (Const c1) (Const c2) ins:rest) = 
    translate memory state acc ([Assign iter (SingleValue (Const (Prelude.max  (c1 - c2 + 1) 0))), loop] ++ rest)
    where
        iter = SingleVar (name++"'")
        name = "-loop"++num
        loop = While (Cond (Var iter) Translator.NE (Const 0)) (Assign iter (Sub (Var iter) (Const 1)):ins)
translate memory state acc (ForDown ('*':num) v1 v2 ins:rest) = 
    translate memory state acc ([Assign iter (Add v1 (Const 1)), Assign iter (Sub (Var iter) v2), loop] ++ rest)
    where
        iter = SingleVar (name++"'")
        name = "-loop"++num
        loop = While (Cond (Var iter) Translator.NE (Const 0)) (Assign iter (Sub (Var iter) (Const 1)):ins)
translate memory state acc ((ForDown n (Const to) (Const from) [Assign (ArrayVar array n') (SingleValue (Const val))]):rest)
    | n == n' = translate' memory state' (code:acc) rest
    | otherwise = translate memory state acc (Assign (ArrayVar array n') (SingleValue (Const val)):rest)
    where
        (state', code) = fillArray memory state rest from to array val
translate memory state acc ((ForDown name v1 v2 ins@(_:_:_:_:_:_)):rest) = 
    translate memory state acc ([Assign pointer (Add v1 (Const 1)), Assign limit (SingleValue v2), loop] ++ rest)
    where
        pointer =  SingleVar name
        limit = SingleVar (name++"'")
        loop = While (Cond (Var pointer) Translator.GT (Var limit)) (Assign pointer (Sub (Var pointer) (Const 1)):ins)
translate memory state acc (ForDown name (Const c1) (Const c2) ins:rest) = 
    translate memory state acc ([Assign iter (SingleValue (Const (Prelude.max (c1 - c2 + 1) 0))), Assign pointer (SingleValue (Const c1)), loop] ++ rest)
    where
        pointer =  SingleVar name
        iter = SingleVar (name++"'")
        loop = While (Cond (Var iter) Translator.NE (Const 0)) (ins++[Assign pointer (Sub (Var pointer) (Const 1)), Assign iter (Sub (Var iter) (Const 1))])
translate memory state acc (ForDown name v1 v2 ins:rest) = 
    translate memory state acc ([Assign iter (Add v1 (Const 1)), Assign iter (Sub (Var iter) v2), Assign pointer (SingleValue v1), loop] ++ rest)
    where
        pointer =  SingleVar name
        iter = SingleVar (name++"'")
        loop = While (Cond (Var iter) Translator.NE (Const 0)) (ins++[Assign pointer (Sub (Var pointer) (Const 1)), Assign iter (Sub (Var iter) (Const 1))])
--translate memory state acc (ForUp name v1 v2 ins:rest) 
--     = traceShow loop $ translate' memory state acc (Assign (SingleVar name) (SingleValue v1):Assign (SingleVar (name++"'")) (SingleValue v2):
--     While (Cond (Var(SingleVar name)) Translator.LE (Var(SingleVar (name++"'")))) (ins ++ [Assign (SingleVar name) (Add (Var(SingleVar name)) (Const 1))]):rest)
--    where
--        loop = Assign (SingleVar name) (SingleValue v1):Assign (SingleVar (name++"'")) (SingleValue v2):
--            While (Cond (Var(SingleVar name)) Translator.LE (Var(SingleVar (name++"'")))) (ins ++ [Assign (SingleVar name) (Add (Var(SingleVar name)) (Const 1))]):[]
--translate memory state acc (ForDown name v1 v2 ins:rest) 
--     = traceShow loop $ translate' memory state acc (Assign (SingleVar name) (SingleValue v1):Assign (SingleVar name) (Add (Var (SingleVar name)) (Const 1)):Assign (SingleVar (name++"'")) (SingleValue v2):
--     While (Cond (Var(SingleVar name)) Translator.GT (Var(SingleVar (name++"'")))) (Assign (SingleVar name) (Sub (Var(SingleVar name)) (Const 1)):ins):rest)
--     where
--         loop = Assign (SingleVar name) (SingleValue v1):Assign (SingleVar name) (Add (Var (SingleVar name)) (Const 1)):Assign (SingleVar (name++"'")) (SingleValue v2):
--            While (Cond (Var(SingleVar name)) Translator.GT (Var(SingleVar (name++"'")))) (Assign (SingleVar name) (Sub (Var(SingleVar name)) (Const 1)):ins):[]
-- IF
translate memory state acc (If cond ifIns:rest) = translate' memory state acc (IfElse cond ifIns []:rest)
translate memory state acc (IfElse cond ifIns elseIns:rest) = case optimizeCond cond of
    Cond v Translator.EQ (Const 0) -> translate' memory state' ((load_code++comp_code++if_out++else_out):acc) rest 
        where
            regs = getRegisters state [v] [] ifIns 
            reg = regs Map.! v
            (load_state, load_code) = loadUnsavedValues memory state [(reg, v)] ifIns
            comp_code = [JZERO reg 2, JUMP (insLength if_out + 1)]
            (after_if_state, after_if_out) = translate' memory load_state [] ifIns
            (after_else_state, after_else_out) = translate' memory load_state [] elseIns
            (state', correct_if, correct_else) = unifyRegiserState memory rest after_if_state after_else_state
            if_out = after_if_out ++ correct_if ++ [JUMP (insLength else_out + 1)]
            else_out = after_else_out ++ correct_else
    Cond v Translator.NE (Const 0) -> translate' memory state' ((load_code++comp_code++if_out++else_out):acc) rest 
        where
            regs = getRegisters state [v] [] ifIns 
            reg = regs Map.! v
            (load_state, load_code) = loadUnsavedValues memory state [(reg, v)] ifIns
            comp_code = [JZERO reg (insLength if_out + 1)]
            (after_if_state, after_if_out) = translate' memory load_state [] ifIns
            (after_else_state, after_else_out) = translate' memory load_state [] elseIns
            (state', correct_if, correct_else) = unifyRegiserState memory rest after_if_state after_else_state
            if_out = after_if_out ++ correct_if ++ [JUMP (insLength else_out + 1)]
            else_out = after_else_out ++ correct_else
    Cond v1 op v2 -> translate' memory state' ((load_code++comp_code++if_out++else_out):acc) rest 
        where
            regs = getRegisters state [v1, v2] [fake1] ifIns 
            reg1 = regs Map.! v1
            reg2 = regs Map.! v2
            regTmp = regs Map.! fake1
            (pre_load_state, load_code) = loadUnsavedValues memory state [(reg1, v1), (reg2, v2), (regTmp, Const 0)] ifIns
            comp_code = compareVarToVar reg1 (oppositeComperator op) reg2 regTmp (insLength if_out+1)
            load_state = Map.insert regTmp [] pre_load_state
            (after_if_state, after_if_out) = translate' memory load_state [] ifIns
            (after_else_state, after_else_out) = translate' memory load_state [] elseIns
            (state', correct_if, correct_else) = unifyRegiserState memory rest after_if_state after_else_state
            if_out = after_if_out ++ correct_if ++ [JUMP (insLength else_out + 1)]
            else_out = after_else_out ++ correct_else
-- WHILE
translate memory state acc (While cond loopIns:rest) = case oppositeCond cond of --opposite Cond
    cond'@(Cond v1 Translator.EQ (Const 0)) -> translate' memory state' (code:acc) rest
        where
            optimal_state = optimalFirstCondLoopRegistersState memory state cond' loopIns            
            regs = getRegisters optimal_state [v1] [] loopIns
            reg1 = regs Map.! v1
            (load_state, load_code) = loadUnsavedValues memory optimal_state [(reg1, v1)] loopIns
            cond_code = [JZERO reg1 (insLength loop_code + insLength correnction_code + 2)]
            fake_cond = [JZERO reg1 0]
            (loop_state, loop_code) = translate' memory load_state [] loopIns
            Just correnction_code = forceRegisterToFit memory loopIns loop_state (removeUnseless (load_code++fake_cond++loop_code) optimal_state) 
            Just loop_fit = forceRegisterToFit memory loopIns state (removeUnseless (load_code++fake_cond++loop_code) optimal_state)
            (state', _) = loadUnsavedValues memory (removeUnseless (load_code++fake_cond++loop_code) optimal_state) [(reg1, v1)] loopIns
            code = loop_fit ++ load_code ++ cond_code ++ loop_code ++ [COMMENT ("CORRECTION CODE "++showRegs optimal_state)]++ correnction_code
                ++ [JUMP (- insLength load_code - insLength cond_code - insLength loop_code - insLength correnction_code)]
    cond'@(Cond v1 Translator.NE (Const 0)) -> translate' memory state' (code:acc) rest
        where
            optimal_state = optimalFirstCondLoopRegistersState memory state cond' loopIns            
            regs = getRegisters optimal_state [v1] [] loopIns
            reg1 = regs Map.! v1
            (load_state, load_code) = loadUnsavedValues memory optimal_state [(reg1, v1)] loopIns
            cond_code = [JZERO reg1 2, JUMP (insLength loop_code + insLength correnction_code + 2)]
            fake_cond = [JZERO reg1 2]
            (loop_state, loop_code) = translate' memory load_state [] loopIns
            Just correnction_code = forceRegisterToFit memory loopIns loop_state (removeUnseless (load_code++fake_cond++loop_code) optimal_state) 
            Just loop_fit = forceRegisterToFit memory loopIns state (removeUnseless (load_code++fake_cond++loop_code) optimal_state)
            (state', _) = loadUnsavedValues memory (removeUnseless (load_code++fake_cond++loop_code) optimal_state) [(reg1, v1)] loopIns
            code = loop_fit ++ load_code ++ cond_code ++ loop_code ++ [COMMENT ("CORRECTION CODE "++showRegs optimal_state)]++ correnction_code
                ++ [JUMP (- insLength load_code - insLength cond_code - insLength loop_code - insLength correnction_code)]
    cond'@(Cond v1 op v2) -> translate' memory state' (code:acc) rest
        where
            optimal_state = optimalFirstCondLoopRegistersState memory state cond' loopIns            
            regs = getRegisters optimal_state [v1, v2] [fake1] loopIns
            reg1 = regs Map.! v1
            reg2 = regs Map.! v2
            regTmp = regs Map.! fake1
            (load_state, load_code) = loadUnsavedValues memory optimal_state [(reg1, v1), (reg2, v2), (regTmp, Const 0)] loopIns
            cond_code = compareVarToVar reg1 op reg2 regTmp (insLength loop_code + insLength correnction_code + 2)
            fake_cond = compareVarToVar reg1 op reg2 regTmp (0)
            cond_state = Map.insert regTmp [] load_state
            (loop_state, loop_code) = translate' memory cond_state [] loopIns
            Just correnction_code = forceRegisterToFit memory loopIns loop_state (removeUnseless (load_code++fake_cond++loop_code) optimal_state) 
            Just loop_fit = forceRegisterToFit memory loopIns state (removeUnseless (load_code++fake_cond++loop_code) optimal_state)
            state' = Map.insert regTmp [] $ fst $ loadUnsavedValues memory (removeUnseless (load_code++fake_cond++loop_code) optimal_state) [(reg1, v1), (reg2, v2), (regTmp, Const 0)] loopIns
            code = loop_fit ++ load_code ++ cond_code ++ loop_code ++ [COMMENT ("CORRECTION CODE "++showRegs optimal_state)]++ correnction_code
                ++ [JUMP (- insLength load_code - insLength cond_code - insLength loop_code - insLength correnction_code)]
--UNTIL
--translate memory state acc (While cond loopIns:rest) = translate' memory state acc (If cond [Until (oppositeCond cond) loopIns]:rest)
translate memory state acc (Until cond loopIns:rest) = case cond of
    cond'@(Cond v1 Translator.EQ (Const 0)) -> translate' memory load_state ((save_code++code++[JUMP (-insLength code)]):acc) rest
        where
            optimal_state = optimalLoopRegistersState memory state cond' loopIns
            (loop_state, loop_code) = translate' memory optimal_state [] loopIns
            regs = getRegisters loop_state [v1] [] loopIns
            reg = regs Map.! v1
            (load_state, load_code) = loadUnsavedValues memory loop_state [(reg, v1)] loopIns
            cond_code = [JZERO reg (insLength correnction_code + 2)]
            Just correnction_code = forceRegisterToFit memory loopIns load_state (removeUnseless (loop_code++load_code++[JZERO reg 0]) optimal_state) 
            Just save_code = forceRegisterToFit memory loopIns state (removeUnseless (loop_code++load_code++[JZERO reg 0]) optimal_state)
            code = loop_code ++ load_code ++ cond_code ++ [COMMENT ("CORRECTION CODE "++showRegs optimal_state)] ++ correnction_code
    cond'@(Cond v1 Translator.NE (Const 0)) -> translate' memory load_state ((save_code++code++[JUMP (-insLength code)]):acc) rest
        where
            optimal_state = optimalLoopRegistersState memory state cond' loopIns
            (loop_state, loop_code) = translate' memory optimal_state [] loopIns
            regs = getRegisters loop_state [v1] [] loopIns
            reg = regs Map.! v1
            (load_state, load_code) = loadUnsavedValues memory loop_state [(reg, v1)] loopIns
            cond_code = [JZERO reg 2, JUMP (insLength correnction_code + 2)]
            Just correnction_code = forceRegisterToFit memory loopIns load_state (removeUnseless (loop_code++load_code++[JZERO reg 0]) optimal_state) 
            Just save_code = forceRegisterToFit memory loopIns state (removeUnseless (loop_code++load_code++[JZERO reg 0]) optimal_state)
            code = loop_code ++ load_code ++ cond_code ++ [COMMENT ("CORRECTION CODE "++showRegs optimal_state)] ++ correnction_code
    cond'@(Cond v1 op v2) -> translate' memory after_cond_state ((save_code++code++[JUMP (-insLength code)]):acc) rest 
        where
            optimal_state = optimalLoopRegistersState memory state cond' loopIns
            (loop_state, loop_code) = translate' memory optimal_state [] loopIns
            regs = getRegisters loop_state [v1, v2] [fake1] loopIns
            reg1 = regs Map.! v1
            reg2 = regs Map.! v2
            regTmp = regs Map.! fake1
            (load_state, load_code) = loadUnsavedValues memory loop_state [(reg1, v1), (reg2, v2), (regTmp, Const 0)] loopIns
            cond_code = compareVarToVar reg1 op reg2 regTmp (insLength correnction_code + 2)
            fake_cond = compareVarToVar reg1 op reg2 regTmp 0
            after_cond_state = Map.insert regTmp [] load_state
            Just correnction_code = forceRegisterToFit memory loopIns after_cond_state (removeUnseless (loop_code++load_code++fake_cond) optimal_state) 
            Just save_code = forceRegisterToFit memory loopIns state (removeUnseless (loop_code++load_code++fake_cond) optimal_state)
            code = loop_code ++ load_code ++ cond_code ++ [COMMENT ("CORRECTION CODE "++showRegs optimal_state)] ++ correnction_code
--ASSIGNMENT
translate memory state acc commands@(Assign id exp:rest) = translate' memory state' (calc_code:code_array_saved:acc) rest
    where
        (prev_state_array_saved, code_array_saved) = saveDependingArrays id memory commands state
        state_array_saved = notifySavedVariable id prev_state_array_saved
        toSave = Var id
        (calc_state_no_reduce, calc_code_no_reduce) = getCalc state_array_saved
        (calc_state_reduce, calc_code_reduce') = getCalc reduce_state
        calc_code_reduce = reduce_code ++ calc_code_reduce'
        (reduce_state, reduce_code) = reduceUnsavedData 3 memory commands state_array_saved
        (state', calc_code) = if numberOfUnsavedRegisters calc_state_no_reduce <= 4 
            then (calc_state_no_reduce, calc_code_no_reduce)
            else (calc_state_reduce, calc_code_reduce)
        getCalc prev_calc_state = case optimizeExp id exp of
            SingleValue v -> (new_state, new_code)
                where
                    regs = getRegisters prev_calc_state [v] [] commands
                    reg = regs Map.! v
                    (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, v)] commands
                    new_state = Map.insertWith (++) reg [unsavedValueToContent (Var id)] (removeDependingContent id s)
            Add v@(Var varId) (Const const) -> if varId == id 
                then case const < 6 of
                    True -> (new_state, new_code ++ genericReplicate const (INC reg))
                        where
                            regs = getRegisters prev_calc_state [v] [] commands
                            reg = regs Map.! v
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, v)] commands
                            new_state = Map.insert reg [unsavedValueToContent v] (removeDependingContent id s)
                    False -> (new_state, new_code ++ [ADD reg reg2])
                        where
                            regs = getRegisters prev_calc_state [v, Const const] [] commands
                            reg = regs Map.! v
                            reg2 = regs Map.! Const const
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, v), (reg2, Const const)] commands
                            new_state = Map.insert reg [unsavedValueToContent v] (removeDependingContent id s)
                else case const < 6 of
                    True -> (new_state, new_code ++ [ADD regRes reg1] ++ genericReplicate const (INC regRes))
                        where
                            regs = getRegisters prev_calc_state [v, toSave] [] commands
                            reg1 = regs Map.! v
                            regRes = regs Map.! toSave
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg1, v), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] (removeDependingContent id s)
                    False -> (new_state, new_code ++ [ADD regRes reg1] ++ [ADD regRes reg2])
                        where
                            regs = getRegisters prev_calc_state [v, Const const, toSave] [] commands
                            reg1 = regs Map.! v
                            reg2 = regs Map.! Const const
                            regRes = regs Map.! toSave
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg1, v), (reg2, Const const), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] (removeDependingContent id s)
            Add (Var varId1) (Var varId2) -> case varId1 == id of
                True -> (new_state, new_code++[ADD reg1 reg2])
                    where
                        regs = getRegisters prev_calc_state [Var varId1, Var varId2] [] commands
                        reg1 = regs Map.! Var varId1
                        reg2 = regs Map.! Var varId2
                        (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg1, Var varId1), (reg2, Var varId2)] commands
                        new_state = Map.insert reg1 [unsavedValueToContent (Var varId1)] (removeDependingContent id s)
                False -> (new_state, new_code ++ [ADD regRes reg1, ADD regRes reg2])
                    where
                        regs = getRegisters prev_calc_state [Var varId1, Var varId2, toSave] [] commands
                        reg1 = regs Map.! Var varId1
                        reg2 = regs Map.! Var varId2
                        regRes = regs Map.! toSave
                        (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg1, Var varId1), (reg2, Var varId2), (regRes, Const 0)] commands
                        new_state = Map.insert regRes [unsavedValueToContent (Var id)] (removeDependingContent id s)
            Add (Const c1) (Const c2) -> (new_state, new_code)
                where
                    regs = getRegisters prev_calc_state [] [Const (c1+c2)] commands
                    reg = regs Map.! Const (c1+c2)
                    (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, Const (c1 + c2))] commands
                    new_state = Map.insert reg [unsavedValueToContent (Var id)] (removeDependingContent id s)

            Sub (Const c) (Var id') -> (new_state, new_code ++ [SUB regRes reg2])
                where
                    regs = getRegisters prev_calc_state [Var id'] [Const c] commands
                    regRes = regs Map.! Const c
                    reg2 = regs Map.! Var id'
                    (s, new_code) = loadUnsavedValues memory prev_calc_state [(regRes, Const c), (reg2, Var id')] commands
                    new_state = Map.insert regRes [unsavedValueToContent (Var id)] (removeDependingContent id s)
            Sub v@(Var id') (Const const) -> if id == id' 
                then case const < 6 of
                    True -> (new_state, new_code ++ genericReplicate const (DEC reg))
                        where
                            regs = getRegisters prev_calc_state [v] [] commands
                            reg = regs Map.! v
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, v)] commands
                            new_state = Map.insert reg [unsavedValueToContent v] (removeDependingContent id s)
                    False -> (new_state, new_code ++ [SUB reg reg2])
                        where
                            regs = getRegisters prev_calc_state [v] [Const const] commands
                            reg = regs Map.! v
                            reg2 = regs Map.! Const const
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, v), (reg2, Const const)] commands
                            new_state = Map.insert reg [unsavedValueToContent v] (removeDependingContent id s)
                else case const < 6 of
                    True -> (new_state, new_code ++ [ADD regRes reg1] ++ genericReplicate const (DEC regRes))
                        where
                            regs = getRegisters prev_calc_state [v] [fake1] commands
                            reg1 = regs Map.! v
                            regRes = regs Map.! fake1
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg1, v), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] (removeDependingContent id s)
                    False -> (new_state, new_code ++ [ADD regRes reg1] ++ [SUB regRes reg2])
                        where
                            regs = getRegisters prev_calc_state [v, Const const] [fake1] commands
                            reg1 = regs Map.! v
                            reg2 = regs Map.! Const const
                            regRes = regs Map.! fake1
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg1, v), (reg2, Const const), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] (removeDependingContent id s)
            Sub (Var varId1) (Var varId2) -> case varId1 == id of
                True -> (new_state, new_code++[SUB reg1 reg2])
                    where
                        regs = getRegisters prev_calc_state [Var varId1, Var varId2] [] commands
                        reg1 = regs Map.! Var varId1
                        reg2 = regs Map.! Var varId2
                        (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg1, Var varId1), (reg2, Var varId2)] commands
                        new_state = Map.insert reg1 [unsavedValueToContent (Var varId1)] (removeDependingContent id s)
                False -> (new_state, new_code ++ [ADD regRes reg1, SUB regRes reg2])
                    where
                        regs = getRegisters prev_calc_state [Var varId1, Var varId2] [fake1] commands
                        reg1 = regs Map.! Var varId1
                        reg2 = regs Map.! Var varId2
                        regRes = regs Map.! fake1
                        (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg1, Var varId1), (reg2, Var varId2), (regRes, Const 0)] commands
                        new_state = Map.insert regRes [unsavedValueToContent (Var id)] (removeDependingContent id s)
            Sub _ _ -> error "Tried sub const to conts"
            Mul v@(Var id') (Const c) -> if id' == id
                then case getPowerOfTwo c of
                    Nothing -> (new_state, new_code ++ multiplyByConst regPrev regRes c)
                        where
                            regs = getRegisters prev_calc_state [Var id'] [fake1] commands
                            regPrev = regs Map.! Var id'
                            regRes = regs Map.! fake1
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(regPrev, Var id'), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert regPrev [] $ removeDependingContent id s
                    Just l -> (new_state, new_code ++ genericReplicate l (SHL reg))
                        where
                            regs = getRegisters prev_calc_state [Var id'] [] commands
                            reg = regs Map.! Var id'
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, Var id')] commands
                            new_state = Map.insert reg [unsavedValueToContent (Var id)] (removeDependingContent id s)
                else case getPowerOfTwo c of
                    Nothing -> (new_state, new_code ++ multiplyByConst reg regRes c)
                        where
                            regs = getRegisters prev_calc_state [] [v, fake1] commands
                            reg = regs Map.! v
                            regRes = regs Map.! fake1
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, v), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert reg [] $ removeDependingContent id s
                    Just l -> (new_state, new_code ++ [ADD regRes reg] ++ genericReplicate l (SHL regRes))
                        where
                            regs = getRegisters prev_calc_state [] [v, fake1] commands
                            reg = regs Map.! v
                            regRes = regs Map.! fake1
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, v), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert reg [] $ removeDependingContent id s
            Mul (Var var1Id) (Var var2Id) -> if var1Id /= var2Id
                then case id == var1Id of
                    True -> (new_state, new_code ++ multiplyRegisters reg1 reg2 regRes)
                        where
                            regs = getRegisters prev_calc_state [Var var1Id] [Var var2Id, fake1] commands
                            reg1 = regs Map.! Var var1Id
                            reg2 = regs Map.! Var var2Id
                            regRes = regs Map.! fake1
                            (s, new_code) = loadPartlyUnsavedValues memory prev_calc_state [(reg2, Var var2Id)] [(reg1, Var var1Id), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert reg1 [] $ Map.insert reg2 [] $ removeDependingContent id s
                    False -> (new_state,  new_code ++ multiplyRegisters reg1 reg2 regRes)
                        where
                            regs = getRegisters prev_calc_state [Var var1Id] [Var var2Id, fake1] commands
                            reg1 = regs Map.! Var var1Id
                            reg2 = regs Map.! Var var2Id
                            regRes = regs Map.! fake1
                            (s, new_code) = savePrevAndLoadValue memory prev_calc_state [(reg2, Var var2Id), (reg1, Var var1Id), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert reg1 [] $ Map.insert reg2 [] $ removeDependingContent id s
                else case id == var1Id of
                    True -> (new_state, new_code ++ multiplyRegisters reg1 reg2 regRes)
                        where
                            regs = getRegisters prev_calc_state [] [Var var1Id, fake1, fake2] commands
                            reg1 = regs Map.! Var var1Id
                            reg2 = regs Map.! fake2
                            regRes = regs Map.! fake1
                            (s, new_code) = loadPartlyUnsavedValues memory prev_calc_state [(reg2, Var var2Id), (reg1, Var var1Id)] [(regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert reg1 [] $ Map.insert reg2 [] $ removeDependingContent id s
                    False -> (new_state,  new_code ++ multiplyRegisters reg1 reg2 regRes)
                        where
                            regs = getRegisters prev_calc_state [] [Var var1Id, fake1, fake2] commands
                            reg1 = regs Map.! Var var1Id
                            reg2 = regs Map.! fake2
                            regRes = regs Map.! fake1
                            (s, new_code) = savePrevAndLoadValue memory prev_calc_state [(reg2, Var var2Id), (reg1, Var var1Id), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert reg1 [] $ Map.insert reg2 [] $ removeDependingContent id s
            Mul _ _ -> error "Two const multiplying"
            Div (Const c) v1 -> (new_state, new_code++divideRegisters reg1 reg2 regRes regTmp) 
                where
                    -- Split for register opimalization 
                    regs = getRegisters prev_calc_state [] [Const c, v1, fake1, fake2] commands
                    reg1 = regs Map.! Const c   
                    reg2 = regs Map.! v1        
                    regRes = regs Map.! fake1   
                    regTmp = regs Map.! fake2   
                    (s, new_code) = savePrevAndLoadValue memory prev_calc_state [(reg1, Const c), (reg2, v1), (regRes, Const 0), (regTmp, Const 0)] commands
                    new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ Map.insert reg1 [] $ Map.insert reg2 [] $ removeDependingContent id s
            Div v@(Var id') (Const const) -> if id' == id
                then case getPowerOfTwo const of
                    Nothing -> (new_state, new_code++divideRegisters reg1 reg2 regRes regTmp)
                        where
                            regs = getRegisters prev_calc_state [v] [Const const, fake1, fake2] commands
                            reg1 = regs Map.! v  
                            reg2 = regs Map.! Const const        
                            regRes = regs Map.! fake1   
                            regTmp = regs Map.! fake2   
                            (s, new_code) = loadPartlyUnsavedValues memory prev_calc_state [(reg1, v)] [(reg2, Const const), (regRes, Const 0), (regTmp, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ Map.insert reg1 [] $ Map.insert reg2 [] $ removeDependingContent id s
                    Just l -> (new_state, new_code ++ genericReplicate l (SHR reg))
                        where
                            regs = getRegisters prev_calc_state [v] [] commands
                            reg = regs Map.! v
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, v)] commands
                            new_state = Map.insert reg [unsavedValueToContent (Var id)] $ removeDependingContent id s
                else case getPowerOfTwo const of
                    Nothing -> (new_state, new_code++divideRegisters reg1 reg2 regRes regTmp)
                        where
                            regs = getRegisters prev_calc_state [] [Const const, v, fake1, fake2] commands
                            reg1 = regs Map.! v  
                            reg2 = regs Map.! Const const         
                            regRes = regs Map.! fake1   
                            regTmp = regs Map.! fake2   
                            (s, new_code) = savePrevAndLoadValue memory prev_calc_state [(reg1, v), (reg2, Const const), (regRes, Const 0), (regTmp, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ Map.insert reg1 [] $ Map.insert reg2 [] $ removeDependingContent id s
                    Just l -> (new_state, new_code ++ [ADD regRes reg] ++ genericReplicate l (SHR regRes))
                        where
                            regs = getRegisters prev_calc_state [] [v, fake1] commands
                            reg = regs Map.! v
                            regRes = regs Map.! fake1
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg, v), (regRes, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ removeDependingContent id s
            Div v1@(Var var1Id) v2@(Var var2Id) -> case var1Id == var2Id of
                True -> (new_state, new_code++code)
                    where
                        regs = getRegisters prev_calc_state [v1] [fake1] commands
                        regRes = regs Map.! fake1
                        reg1 = regs Map.! v1
                        (s, new_code) = loadPartlyUnsavedValues memory prev_calc_state [(reg1, v1)] [(regRes, Const 0)] commands
                        code = [JZERO reg1 2, INC regRes]
                        new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ removeDependingContent id s
                False -> case id == var1Id of 
                    True -> (new_state, new_code++divideRegisters reg1 reg2 regRes regTmp)
                        where
                            regs = getRegisters prev_calc_state [v1] [v2, fake1, fake2] commands
                            reg1 = regs Map.! v1 
                            reg2 = regs Map.! v2        
                            regRes = regs Map.! fake1   
                            regTmp = regs Map.! fake2 
                            (s, new_code) = loadPartlyUnsavedValues memory prev_calc_state [(reg1, v1)] [(reg2, v2), (regRes, Const 0), (regTmp, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ Map.insert reg1 [] $ Map.insert reg2 [] $ removeDependingContent id s
                    False -> (new_state, new_code++divideRegisters reg1 reg2 regRes regTmp)
                        where
                            regs = getRegisters prev_calc_state [] [v1, v2, fake1, fake2] commands
                            reg1 = regs Map.! v1 
                            reg2 = regs Map.! v2        
                            regRes = regs Map.! fake1   
                            regTmp = regs Map.! fake2 
                            (s, new_code) = savePrevAndLoadValue memory prev_calc_state [(reg1, v1), (reg2, v2), (regRes, Const 0), (regTmp, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ Map.insert reg1 [] $ Map.insert reg2 [] $ removeDependingContent id s
            Mod (Const c) v1 -> (new_state, new_code++moduloRegisters reg1 reg2 reg3 regTmp) 
                where
                    -- Split for register opimalization 
                    regs = getRegisters prev_calc_state [] [Const c, v1, fake1, fake2] commands
                    reg1 = regs Map.! Const c   
                    reg2 = regs Map.! v1        
                    reg3 = regs Map.! fake1   
                    regTmp = regs Map.! fake2   
                    (s, new_code) = savePrevAndLoadValue memory prev_calc_state [(reg1, Const c), (reg2, v1), (reg3, Const 0), (regTmp, Const 0)] commands
                    new_state = Map.insert reg1 [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ Map.insert reg2 [] $ Map.insert reg3 [] $ removeDependingContent id s
            Mod v1@(Var id') (Const const) -> if id == id' 
                then case getPowerOfTwo const of
                    Nothing -> (new_state, new_code++moduloRegisters reg1 reg2 reg3 regTmp)
                        where
                            regs = getRegisters prev_calc_state [v1] [Const const, fake1, fake2] commands
                            reg1 = regs Map.! v1 
                            reg2 = regs Map.! Const const        
                            reg3 = regs Map.! fake1   
                            regTmp = regs Map.! fake2 
                            (s, new_code) = loadPartlyUnsavedValues memory prev_calc_state [(reg1, v1)] [(reg2, Const const ), (reg3, Const 0), (regTmp, Const 0)] commands
                            new_state = Map.insert reg1 [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ Map.insert reg2 [] $ Map.insert reg3 [] $ removeDependingContent id s
                    Just l -> (new_state, new_code++[ADD regTmp reg1]++genericReplicate l (SHR regTmp)++genericReplicate l (SHL regTmp)++[SUB reg1 regTmp])
                        where
                            regs = getRegisters prev_calc_state [v1] [fake1] commands 
                            reg1 = regs Map.! v1 
                            regTmp = regs Map.! fake1 
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg1, v1), (regTmp, Const 0)] commands
                            new_state = Map.insert reg1 [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ removeDependingContent id s
                else case getPowerOfTwo const of
                    Nothing -> (new_state, new_code++moduloRegisters reg1 reg2 reg3 regTmp)
                        where
                            regs = getRegisters prev_calc_state [] [v1, Const const, fake1, fake2] commands
                            reg1 = regs Map.! v1 
                            reg2 = regs Map.! Const const        
                            reg3 = regs Map.! fake1   
                            regTmp = regs Map.! fake2 
                            (s, new_code) = savePrevAndLoadValue memory prev_calc_state [(reg1, v1), (reg2, Const const ), (reg3, Const 0), (regTmp, Const 0)] commands
                            new_state = Map.insert reg1 [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ Map.insert reg2 [] $ Map.insert reg3 [] $ removeDependingContent id s
                    Just l -> (new_state, new_code++[ADD regRes reg1, ADD regTmp reg1]++genericReplicate l (SHR regTmp)++genericReplicate l (SHL regTmp)++[SUB regRes regTmp])
                        where
                            regs = getRegisters prev_calc_state [] [v1, fake1, fake2] commands
                            reg1 = regs Map.! v1
                            regRes = regs Map.! fake1  
                            regTmp = regs Map.! fake2
                            (s, new_code) = loadUnsavedValues memory prev_calc_state [(reg1, v1), (regRes, Const 0), (regTmp, Const 0)] commands
                            new_state = Map.insert regRes [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ removeDependingContent id s
            Mod v1@(Var var1Id) v2@(Var _) -> case id == var1Id of
                True -> (new_state, new_code++moduloRegisters reg1 reg2 reg3 regTmp) 
                    where
                        regs = getRegisters prev_calc_state [v1] [v2, fake1, fake2] commands
                        reg1 = regs Map.! v1  
                        reg2 = regs Map.! v2        
                        reg3 = regs Map.! fake1   
                        regTmp = regs Map.! fake2   
                        (s, new_code) = loadPartlyUnsavedValues memory prev_calc_state [(reg1, v1)] [(reg2, v2), (reg3, Const 0), (regTmp, Const 0)] commands
                        new_state = Map.insert reg1 [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ Map.insert reg2 [] $ Map.insert reg3 [] $ removeDependingContent id s
                False -> (new_state, new_code++moduloRegisters reg1 reg2 reg3 regTmp) 
                    where
                        regs = getRegisters prev_calc_state [] [v1, v2, fake1, fake2] commands
                        reg1 = regs Map.! v1  
                        reg2 = regs Map.! v2        
                        reg3 = regs Map.! fake1   
                        regTmp = regs Map.! fake2   
                        (s, new_code) = savePrevAndLoadValue memory prev_calc_state [(reg1, v1), (reg2, v2), (reg3, Const 0), (regTmp, Const 0)] commands
                        new_state = Map.insert reg1 [unsavedValueToContent (Var id)] $ Map.insert regTmp [] $ Map.insert reg2 [] $ Map.insert reg3 [] $ removeDependingContent id s


                
