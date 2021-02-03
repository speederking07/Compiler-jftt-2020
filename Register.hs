module Register where

import Parser
import Translator
import Memory hiding (sortByUsefullnes)
import Data.Map ((!))
import qualified Data.Map as Map 
import qualified Control.Arrow as Data.Bifunctor
import qualified Data.List as List
import qualified Control.Monad as Monad
import Data.Tuple (swap)

import Debug
import Debug.Trace

data Content = Constant Integer | Variable Identifier Bool deriving (Eq, Ord)
-- False means unsaved 
type RegisterState = Map.Map Register [Content]

showRegs :: (Show a1, Show (t a2), Foldable t) => Map.Map a1 (t a2) -> [Char]
showRegs regs = Map.foldlWithKey (\prev r c-> prev++show r++" - "++show c++" ") "" (Map.filter (not.null) regs) 

instance Show Content where
    show (Constant c) = show c
    show (Variable v True) = show v ++ " saved" 
    show (Variable v False) = show v ++ " unsaved"



clearState :: RegisterState
clearState = Map.fromList [(A, []), (B, []), (C, []), (D, []), (E, []), (F, [])]

allRegisters :: [Register]
allRegisters = [A,B,C,D,E,F]

unifyRegiserState :: MemoryStructure -> [Command] -> RegisterState -> RegisterState -> (RegisterState, [Output], [Output])
unifyRegiserState memory commands reg1 reg2 = (targetReg, code1++code1', code2++code2')
    where
        listOfCommonValue = concatMap (fmap contentToValue . snd) (Map.toList reg1) `List.intersect` concatMap (fmap contentToValue . snd) (Map.toList reg1)
        preparedList = take 4 $ List.filter (\x -> whenValueUsed commands x <= 3) listOfCommonValue
        unFoldList :: [(a, [b])] -> [(a,b)]
        unFoldList [] = []
        unFoldList ((_, []):r) = unFoldList r
        unFoldList ((x, y:ys):r) = (x,y):unFoldList ((x, ys):r)
        isSaved (Variable _ t) = t
        isSaved _ = True
        toSave :: [(Register, Content)] -> [(Register, Content)]
        toSave = List.filter (\(_,c) -> not (contentToValue c `elem` preparedList || isSaved c)) 
        (state1, code1) = saveRegistersContent memory reg1 (toSave $ unFoldList $ Map.toList reg1) commands
        (state2, code2) = saveRegistersContent memory reg2 (toSave $ unFoldList $ Map.toList reg2) commands
        deleteUseless reg = fmap (filter (\x -> contentToValue x `elem` preparedList)) reg
        (targetReg, code1') = flattenRegistersState memory (deleteUseless state1) commands
        Just code2' = forceRegisterToFit memory commands state2 targetReg

usedRegisters :: RegisterState -> Integer
usedRegisters = Map.foldl (\i v -> if not(null v) then i + 1 else i) 0 

capContent :: Integer -> [Command] -> RegisterState -> RegisterState
capContent max commands state
    | toDelete <= 0 = state
    | otherwise = List.foldl (\s r -> Map.insert r [] s) state regsToDel
    where
        toDelete = max - usedRegisters state
        regsToDel = List.genericTake toDelete $ sortByUsefullnes commands (Map.toList (Map.filter isSaved state))

forceRegisterToFit :: MemoryStructure -> [Command] -> RegisterState -> RegisterState -> Maybe [Output]
forceRegisterToFit memory commands prev target
    | length state > 5 = Nothing
    | otherwise = Just $ save_code ++ load_code
    where
        (saved_state, save_code) = saveRegistersState memory prev toSave commands
        (_, load_code) = loadPartlyUnsavedValues memory saved_state unsavedList savedList commands
        toSave = fst <$> Map.toList (Map.filter null target)
        state =  head <$> Map.filter (not . null) target
        savedList = Map.toList $ contentToValue <$> Map.filter saved state
        unsavedList = Map.toList $ contentToValue <$> Map.filter (not. saved) state
        saved :: Content -> Bool
        saved (Variable _ x) = x
        saved _ = True

flattenRegistersState :: MemoryStructure -> RegisterState -> [Command] -> (RegisterState, [Output])
flattenRegistersState memory state commands
    | null notFlaten = (flatten, [])
    | otherwise = (flattenv2, code)
    where
        noneVarState = deleteNoneVar state
        (flatten, notFlaten) = tryFlatten noneVarState
        (saved, code) = saveRegistersState memory flatten notFlaten commands
        (flattenv2, []) = tryFlatten saved
        deleteNoneVar :: RegisterState -> RegisterState
        deleteNoneVar = fmap (filter isVar)
            where
                isVar (Variable _ _) = True
                isVar _ = False
        tryFlatten :: RegisterState -> (RegisterState, [Register])
        tryFlatten state = Map.foldrWithKey' removeExtra (state, []) state
        removeExtra :: Register -> [Content] -> (RegisterState, [Register]) -> (RegisterState, [Register])
        removeExtra reg content (prevState, regs) 
            | length content <= 1 = (prevState, regs)
            | not (any noneSaved content) = (Map.insert reg [head content] prevState, regs)
            | length (filter noneSaved content) == 1 = (Map.insert reg (filter noneSaved content) prevState, regs)
            | otherwise = (prevState, reg:regs)
        noneSaved (Variable _ False) = True
        noneSaved _ = False

removeUnseless :: [Output] -> RegisterState -> RegisterState
removeUnseless input state = Map.foldlWithKey' check state state 
    where 
        usefulness = checkUsefulness input
        check :: RegisterState -> Register -> [Content] -> RegisterState
        check s r _ = case usefulness Map.! r of
            Just False -> Map.insert r [] s
            _ -> s

checkUsefulness :: [Output] -> Map.Map Register (Maybe Bool)
checkUsefulness = go (Map.fromList[(A, Nothing), (B, Nothing), (C, Nothing), (D, Nothing), (E, Nothing), (F, Nothing)])
    where
        go acc [] = acc
        go acc (x:xs) = case x of
            GET x -> go (Map.insertWith (flip Monad.mplus) x (Just True) acc) xs
            PUT x -> go (Map.insertWith (flip Monad.mplus) x (Just True) acc) xs
            LOAD x y -> go (Map.insertWith (flip Monad.mplus) x (Just False) (Map.insertWith (flip Monad.mplus) y (Just True) acc)) xs
            STORE x y -> go (Map.insertWith (flip Monad.mplus) x (Just True) (Map.insertWith (flip Monad.mplus) y (Just True) acc)) xs
            ADD x y -> go (Map.insertWith (flip Monad.mplus) x (Just True) (Map.insertWith (flip Monad.mplus) y (Just True) acc)) xs
            SUB x y -> go (Map.insertWith (flip Monad.mplus) x (Just True) (Map.insertWith (flip Monad.mplus) y (Just True) acc)) xs
            RESET x -> go (Map.insertWith (flip Monad.mplus) x (Just False) acc) xs
            INC x -> go (Map.insertWith (flip Monad.mplus) x (Just True) acc) xs
            DEC x -> go (Map.insertWith (flip Monad.mplus) x (Just True) acc) xs
            SHR x -> go (Map.insertWith (flip Monad.mplus) x (Just True) acc) xs
            SHL x -> go (Map.insertWith (flip Monad.mplus) x (Just True) acc) xs
            JZERO x _ -> go (Map.insertWith (flip Monad.mplus) x (Just True) acc) xs
            JODD x _ -> go (Map.insertWith (flip Monad.mplus) x (Just True) acc) xs
            _ -> go acc xs

isSaved :: [Content] -> Bool
isSaved [] = True
isSaved (x:xs) = case x of    
    Variable _ False -> False
    _ -> isSaved xs

deleteMaybe :: [Maybe a] -> [a]
deleteMaybe [] = []
deleteMaybe (Just x:xs) = x:deleteMaybe xs
deleteMaybe (Nothing:xs) = deleteMaybe xs

takeMaybe :: (a, Maybe b) -> Maybe (a, b)
takeMaybe (_, Nothing) = Nothing
takeMaybe (a, Just b) = Just (a, b)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

registersToSaveInOrderToWrite :: RegisterState -> Identifier -> [Register]
registersToSaveInOrderToWrite state id@(SingleVar _) = case findInRegisters state (Var id) of
    Nothing -> []
    Just reg -> [reg]
registersToSaveInOrderToWrite state id@(ArrayConst name index) = fst <$> Map.toList regs
    where
        regs = Map.filter (any depends) state
        depends (Variable (ArrayConst name' index') False) = name == name' && index == index'
        depends (Variable (ArrayVar name' _) False) = name == name'
        depends _ = False
registersToSaveInOrderToWrite state id@(ArrayVar name _) = fst <$> Map.toList regs
    where
        regs = Map.filter (any depends) state
        depends (Variable (ArrayConst name' _) False) = name == name'
        depends (Variable (ArrayVar name' _) False) = name == name'
        depends _ = False

numberOfUnsavedRegisters :: RegisterState -> Integer
numberOfUnsavedRegisters = (6-) . Map.foldl (\a b -> if isContentSaved b then a + 1 else a) 0

reduceUnsavedData :: Integer -> MemoryStructure -> [Command] -> RegisterState -> (RegisterState, [Output])
reduceUnsavedData max_unsaved memory futureCommands state
    | toSave > max_unsaved = saveRegistersState  memory state (List.genericTake (toSave - max_unsaved) regesToBeSaves) futureCommands
    | otherwise = (state, [])
    where
        unsaved = Map.foldl (\a b -> if isContentSaved b then a + 1 else a) 0 state
        toSave = 6 - unsaved
        regesToBeSaves = sortByWhenOverwritten futureCommands (Map.toList $ Map.filter (not . isContentSaved) state)

isContentSaved :: [Content] -> Bool
isContentSaved [] = True
isContentSaved (Variable _ False:_) = False
isContentSaved (_:rest) = isContentSaved rest

saveDependingArrays :: Identifier -> MemoryStructure -> [Command] -> RegisterState -> (RegisterState, [Output])
saveDependingArrays id memory futureCommands state = (state', code)
    where
        content = concat $ expand <$> Map.toList state
        expand (a, bx) = (\b -> (a,b)) <$> bx
        sameId id (Variable id' _) = id == id'
        sameId _ _ = False
        dependingContent = List.filter (\(r,c) -> depend id c && not (sameId id c)) content
        contentToSave = List.filter (\(_, Variable _ saved) -> not saved) dependingContent
        (state', code) = saveRegistersContent memory state contentToSave futureCommands
        
removeDependingContent :: Identifier -> RegisterState -> RegisterState
removeDependingContent id = fmap (List.filter (not . depend id))

depend :: Identifier -> Content -> Bool
depend (SingleVar str) (Variable (SingleVar str') _) = str == str'
depend (SingleVar str) (Variable (ArrayVar _ str') _) = str == str'
depend (ArrayConst str int) (Variable (ArrayConst str' int') _) = str == str' && int == int'
depend (ArrayConst str _) (Variable (ArrayVar str' _) _) = str == str'
depend (ArrayVar str _) (Variable (ArrayConst str' _) _) = str == str'
depend (ArrayVar str var) (Variable (ArrayVar str' var') _) = str == str'
depend _ _ = False       

notifySavedVariable :: Identifier -> RegisterState -> RegisterState
notifySavedVariable id = fmap (fmap (save id))
    where
        save id' v@(Variable id'' _)
            | id' == id'' = Variable id' True
            | otherwise = v
        save _ v = v

findInRegisters :: RegisterState -> Value -> Maybe Register
findInRegisters state value = fmap fst $ maybeHead $ Map.toList state'
    where 
        state' = Map.filter (\content -> value `elem` fmap contentToValue content) state 

isInRegister :: RegisterState -> Register -> Value -> Bool
isInRegister state reg val = val `elem` fmap contentToValue (state!reg)

checkRegistersState :: RegisterState -> [(Register, Value)] -> Bool
checkRegistersState _ [] = True
checkRegistersState state ((r, v):xs)
    | isInRegister state r v = checkRegistersState state xs
    | otherwise = False

loadUnsavedValues :: MemoryStructure -> RegisterState -> [(Register, Value)] -> [Command] -> (RegisterState, [Output])
loadUnsavedValues memory state regs = loadPartlyUnsavedValues memory state regs []


loadPartlyUnsavedValues :: MemoryStructure -> RegisterState -> [(Register, Value)] -> [(Register, Value)] -> [Command] -> (RegisterState, [Output])
loadPartlyUnsavedValues memory state unsafe safe futureCommands
    | checkRegistersState state' (unsafe++safe) = (state', code)
    | otherwise = savePrevAndLoadValue memory state (unsafe++safe) futureCommands
    where
        checkRegister :: RegisterState -> Register -> Value -> Bool
        checkRegister s r v = List.foldl (\a b -> a && ok b v) True (s!r)
        ok :: Content -> Value -> Bool
        ok (Variable id False) (Var id') = id == id'
        ok _ _ = True
        splitRegisters :: RegisterState -> [(Register, Value)] -> ([(Register, Value)], [(Register, Value)])
        splitRegisters _ [] = ([], [])
        splitRegisters reg_state ((r, v):xs) 
            | checkRegister reg_state r v && isInRegister reg_state r v = ((r,v):rest, rest')
            | otherwise = (rest, (r,v):rest')
            where
                (rest, rest') =  splitRegisters reg_state xs
        (_, toChange) = splitRegisters state unsafe
        (state', code) = savePrevAndLoadValue memory state (toChange++safe) futureCommands
        

-- Map.foldl (\a b -> if isContentSaved b then a + 1 else a) 0 state
cloneRegisters :: RegisterState -> Register -> Register -> (RegisterState, [Output])
cloneRegisters state from to = (state', code)
    where
        state' = Map.insert to (state!from) state
        code = [RESET to, ADD to from]

savePrevAndLoadValue :: MemoryStructure -> RegisterState -> [(Register, Value)] -> [Command] -> (RegisterState, [Output])
savePrevAndLoadValue memory regs listOfDataToLoad listOfCommands 
    | not (null uselessRegs) = (state', code++code2)
    | otherwise = (alt_state, reduced_code++alt_code)
    where
        (alt_state, alt_code) = savePrevAndLoadValue memory reduced_state listOfDataToLoad listOfCommands
        (reduced_state, reduced_code) = reduceUnsavedData 0 memory listOfCommands regs
        (state, code) = saveRegistersState memory regs (fmap fst listOfDataToLoad) listOfCommands
        (state', code2) = go state [] listOfDataToLoad
        uselessRegs = sortByUsefullnes listOfCommands (Map.toList (Map.filter isSaved state)) List.\\ fmap fst listOfDataToLoad
        reg:_ = uselessRegs
        constCopy :: Value -> Maybe a -> Maybe a
        constCopy (Const n) m
            | n < 12 = Nothing
            | otherwise = m
        constCopy _ m = m
        go :: RegisterState -> [[Output]] -> [(Register, Value)] -> (RegisterState, [Output])
        go state acc [] = (state, concat $ reverse acc)
        go state acc ((register, value):rest) = case constCopy value $ findInRegisters state value of 
            Just regWithVal -> if regWithVal == register then go state acc rest
                               else go state' (code:acc) rest
                where
                    (state', code) = cloneRegisters state regWithVal register
            Nothing -> case value of
                Const const -> go state' (code:acc) rest
                    where
                        state' = Map.insert register [Constant const] state
                        code = case getRegisterConst state register of
                            Nothing -> generateConsFromBegin const register
                            Just prev -> bestGenerateConst prev const register

                Var(SingleVar name) -> go state' (code:acc) rest
                    where
                        address = memory!name
                        state' = Map.insert register [Variable (SingleVar name) True] state
                        code = case getRegisterConst state register of
                            Nothing -> generateConsFromBegin address register ++ [LOAD register register]
                            Just prev -> bestGenerateConst prev address register ++ [LOAD register register]
                Var(ArrayConst name pos) -> go state' (code:acc) rest
                    where
                        address = memory!name
                        state' = Map.insert register [Variable (ArrayConst name pos) True] state
                        code = case getRegisterConst state register of
                            Nothing -> generateConsFromBegin (address+pos) register ++ [LOAD register register]
                            Just prev -> bestGenerateConst prev (address+pos) register ++ [LOAD register register]
                Var(ArrayVar name var) -> case findInRegisters state (Var(SingleVar var)) of
                    Nothing -> go state' ((code1++code2):acc) rest
                        where
                            address = memory!name
                            varAddress = memory!var
                            state' = Map.insert register [Variable (ArrayVar name var) True] (Map.insert reg [Variable (SingleVar var) True] state)
                            code1 = case getRegisterConst state reg of
                                    Nothing -> generateConsFromBegin varAddress reg ++ [LOAD reg reg]
                                    Just prev -> bestGenerateConst prev varAddress reg ++ [LOAD reg reg]
                            code2 = case getRegisterConst state register of
                                Nothing -> generateConsFromBegin address register ++ [ADD register reg, LOAD register register]
                                Just prev -> bestGenerateConst prev address register ++ [ADD register reg, LOAD register register]
                    Just regWithVar -> case regWithVar == register of
                        True -> go state' (code:acc) rest
                            where
                                address = memory!name
                                state' = Map.insert register [Variable (ArrayVar name var) True] $ Map.insert reg [Constant address] state
                                code = case getRegisterConst state reg of
                                    Nothing -> generateConsFromBegin address reg ++ [ADD register reg, LOAD register register]
                                    Just prev -> bestGenerateConst prev address reg ++ [ADD register reg, LOAD register register]
                        False -> go state' (code:acc) rest
                            where
                                address = memory!name
                                state' = Map.insert register [Variable (ArrayVar name var) True] state
                                code = case getRegisterConst state register of
                                    Nothing -> generateConsFromBegin address register ++ [ADD register regWithVar, LOAD register register]
                                    Just prev -> bestGenerateConst prev address register ++ [ADD register regWithVar, LOAD register register]

loadAddressToRegister :: MemoryStructure -> RegisterState -> Identifier -> Register -> [Command] -> (RegisterState, [Output])
loadAddressToRegister memory state id reg futureCommands = case id of
    SingleVar name -> (state', save_code++code)
        where
            address = memory!name
            (save_state, save_code) = saveRegistersState memory state [reg] futureCommands
            state' = Map.insert reg [Constant address] save_state
            code = case getRegisterConst save_state reg of
                Nothing -> generateConsFromBegin address reg
                Just prev -> bestGenerateConst prev address reg
    ArrayConst name pos -> (state', save_code++code)
        where
            address = memory!name
            (save_state, save_code) = saveRegistersState memory state [reg] futureCommands
            state' = Map.insert reg [Constant (address+pos)] save_state
            code = case getRegisterConst save_state reg of
                Nothing -> generateConsFromBegin (address+pos) reg
                Just prev -> bestGenerateConst prev (address+pos) reg
    ArrayVar name var -> (state', save_code++load_code++code)
        where
            address = memory!name                      
            (save_state, save_code) = saveRegistersState memory state [reg] futureCommands
            bestReg = getRegisters save_state [Var (SingleVar var)] [] futureCommands ! Var (SingleVar var)
            regTmp = if bestReg == reg
                then head $ sortByUsefullnes futureCommands (Map.toList $ Map.filter isSaved save_state) List.\\ [reg] 
                else bestReg
            (load_state, load_code) = loadUnsavedValues memory save_state [(regTmp, Var(SingleVar var))] futureCommands
            code = case getRegisterConst load_state reg of
                Nothing -> generateConsFromBegin address reg ++ [ADD reg regTmp]
                Just prev -> bestGenerateConst prev address reg ++ [ADD reg regTmp]
            state' = Map.insert reg [] load_state

saveRegistersState :: MemoryStructure -> RegisterState -> [Register] -> [Command] -> (RegisterState, [Output])
saveRegistersState memory state regs commands = saveRegistersContent memory state (listToSave state regs) commands
    where
        (t, _) = saveRegistersContent memory state (listToSave state regs) commands --REmove
        listToSave :: RegisterState -> [Register] -> [(Register, Content)]
        listToSave state = foldl (\a b -> a++ foldListToSaveForRegister b state) []
        foldListToSaveForRegister :: Register -> RegisterState -> [(Register, Content)]
        foldListToSaveForRegister reg state = foldl (\a b -> case b of
            Variable _ False -> (reg, b):a 
            _ ->  a) [] (state!reg)

saveRegistersContent :: MemoryStructure -> RegisterState -> [(Register, Content)] -> [Command] -> (RegisterState, [Output])
saveRegistersContent memory regs listOfRegisters listOfCommands = go regs [] list
    where
        regsToUse = sortByUsefullnes listOfCommands $ Map.toList $ Map.filter isSaved regs
        reg1:reg2:_ =  regsToUse
        list = reverse $ msort (\(_, Variable id _) -> idToCost id) getUniqueContent
        idToCost id = case id of
            SingleVar str -> memory!str
            ArrayConst str int -> memory!str + int
            ArrayVar str str' -> memory!str + memory!str' + 6
        getUniqueContent = map swap $ Map.toList $ Map.fromList $ map swap listOfRegisters
        go :: RegisterState -> [[Output]] -> [(Register, Content)] -> (RegisterState, [Output])
        go state acc [] = (state, concat $ reverse acc)
        go state acc ((register, Variable (SingleVar name) False):rest) = go state' (code:acc) rest
            where
                address = memory!name
                state' = Map.insert reg1 [Constant address] (notifySavedVariable (SingleVar name) state)
                code = case getRegisterConst state reg1 of
                    Nothing -> generateConsFromBegin address reg1 ++ [STORE register reg1]
                    Just prev -> bestGenerateConst prev address reg1 ++ [STORE register reg1]
        go state acc ((register, Variable (ArrayConst name pos) False):rest) = go state' (code:acc) rest
            where
                address = memory!name
                state' = Map.insert reg1 [Constant (address+pos)] (notifySavedVariable (ArrayConst name pos) state)
                code = case getRegisterConst state reg1 of
                    Nothing -> generateConsFromBegin (address+pos) reg1 ++ [STORE register reg1]
                    Just prev -> bestGenerateConst prev (address+pos) reg1 ++ [STORE register reg1]
        go state acc ((register, Variable (ArrayVar name var) False):rest) = case findInRegisters state (Var(SingleVar var)) of
            Nothing -> go state' ((code1++code2):acc) rest
                where
                    address = memory!name
                    varAddress = memory!var
                    state' = Map.insert reg2 [Variable (SingleVar var) True] (Map.insert reg1 [] (notifySavedVariable (ArrayVar name var) state))
                    code1 = case getRegisterConst state reg2 of
                            Nothing -> generateConsFromBegin varAddress reg2 ++ [LOAD reg2 reg2]
                            Just prev -> bestGenerateConst prev varAddress reg2 ++ [LOAD reg2 reg2]
                    code2 = case getRegisterConst state reg1 of
                        Nothing -> generateConsFromBegin address reg1 ++ [ADD reg1 reg2, STORE register reg1]
                        Just prev -> bestGenerateConst prev address reg1 ++ [ADD reg1 reg2, STORE register reg1]
            Just regWithVar -> case regWithVar == reg1 of
                True -> go state' (code:acc) rest
                    where
                        address = memory!name
                        state' = Map.insert reg2 [] (notifySavedVariable (ArrayVar name var) state)
                        code = case getRegisterConst state reg2 of
                            Nothing -> generateConsFromBegin address reg2 ++ [ADD reg2 reg1, STORE register reg2]
                            Just prev -> bestGenerateConst prev address reg2 ++ [ADD reg2 reg1, STORE register reg2]
                False -> go state' (code:acc) rest
                    where
                        address = memory!name
                        state' = Map.insert reg1 [] (notifySavedVariable (ArrayVar name var) state)
                        code = case getRegisterConst state reg1 of
                            Nothing -> generateConsFromBegin address reg1 ++ [ADD reg1 regWithVar, STORE register reg1]
                            Just prev -> bestGenerateConst prev address reg1 ++ [ADD reg1 regWithVar, STORE register reg1]

getRegisterConst :: RegisterState -> Register -> Maybe Integer
getRegisterConst state reg = foldConst (state!reg)
     where
        foldConst :: [Content] -> Maybe Integer
        foldConst [] = Nothing
        foldConst (Constant int:_) = Just int
        foldConst (_:rest) = foldConst rest 

--TODO
getRegisters :: RegisterState -> [Value] -> [Value] -> [Command] -> Map.Map Value Register
getRegisters state contentToRead contentToReplace futureCommands = Map.fromList (with_fit_1 ++ with_fit_2 ++ with_fit_3 ++ with_leftover)
    where
        goThrough :: [Value] -> [(Register, [Content])] -> (Value -> [(Register, [Content])] -> Maybe Register) -> ([Value], [(Value, Register)], [(Register, [Content])])
        goThrough [] state _ = ([], [], state)
        goThrough (c:cs) state f = case f c state of
            Nothing -> (c:n_rest, n_rest', finalState)
                where
                    (n_rest, n_rest', finalState) = goThrough cs state f
            Just reg -> (j_rest, (c, reg):j_rest', finalState)
                where
                    state' = List.filter (\(r, _) -> r /= reg) state
                    (j_rest, j_rest', finalState) = goThrough cs state' f
        safeGoThrough [] state _ = ([], [], state)
        safeGoThrough (c:cs) state f = case f c state of
            Nothing -> (c:n_rest, n_rest', finalState)
                where
                    (n_rest, n_rest', finalState) = goThrough cs state f
            Just reg -> if whenRegisterUsed futureCommands (Map.fromList state) reg < 25
                then (c:n_rest, n_rest', finalState)
                else (j_rest, (c, reg):j_rest', finalState')
                where
                    (n_rest, n_rest', finalState) = goThrough cs state f
                    state' = List.filter (\(r, _) -> r /= reg) state
                    (j_rest, j_rest', finalState') = goThrough cs state' f
        (without_fit_1, with_fit_1, state_1) = goThrough contentToRead (Map.toList state) findFittingRegister
        (without_fit_2, with_fit_2, state_2) = safeGoThrough contentToReplace state_1 findFittingRegister
        (without_fit_3, with_fit_3, state_3) = safeGoThrough without_fit_2 state_2 findFittingConst
        leftoverRegisters = sortByUsefullnes futureCommands state_3
        with_leftover = zip (without_fit_3++without_fit_1) leftoverRegisters 

sortByUsefullnes :: [Command] -> [(Register, [Content])] -> [Register]
sortByUsefullnes commands list = fmap first sorted
    where
        listToSort = fmap (\(r, l') -> (r, foldl min 25 (fmap (whenValueUsed commands . contentToValue) l'))) list
        sorted = msort second listToSort
        second (_, x) = x
        first (x, _) = x

sortByWhenOverwritten :: [Command] -> [(Register, [Content])] -> [Register]
sortByWhenOverwritten commands list = fmap first sorted
    where
        listToSort = fmap (\(r, l') -> (r, foldl min 25 (whenIdOverwritten commands . contentToId <$> List.filter isVar l'))) list
        contentToId (Variable id _) = id
        isVar (Variable _ _) = True 
        isVar _ = False 
        sorted = msort second listToSort
        second (_, x) = x
        first (x, _) = x

contentToValue :: Content -> Value
contentToValue (Constant c) = Const c
contentToValue (Variable id _) = Var id

whenRegisterUsed :: [Command] -> RegisterState -> Register -> Integer
whenRegisterUsed commands state reg = foldl min 25 (fmap (whenValueUsed commands . contentToValue) (state!reg)) 

whenValueUsed :: [Command] -> Value -> Integer
whenValueUsed commands value = go 0 value commands
    where
        go 5 _ _ = 5
        go _ _ [] = 10
        go n v (Assign id exp:rest) 
            | inExp v exp = n
            | isId v id = 20
            | otherwise = go (n+1) v rest
        go n v (IfElse cond ins ins':rest)
            | inCond v cond = n
            | otherwise = min (go (n+1) v (ins++rest)) (go (n+1) v (ins'++rest))
        go n v (If cond ins:rest)
            | inCond v cond = n
            | otherwise = min (go (n+1) v ins) (go (n+1) v rest)
        go n v (While cond ins:rest)
            | inCond v cond = n
            | otherwise = go (n+1) v ins
        go n v (Until cond ins:rest)
            | inCond v cond = n
            | otherwise = go (n+1) v ins
        go n v (ForUp str _ _ ins:rest) = if value /= Var (SingleVar (str++"'"))
            then go (n+1) v ins
            else n
        go n v (ForDown str _ _ ins:rest) = if value /= Var (SingleVar (str++"'"))
            then go (n+1) v ins
            else n
        go n v (Read id:rest)
            | isId v id = 10 
            | otherwise = go (n+1) v rest
        go n v (Write v':rest)
            | inVal v v' = n
            | otherwise = go (n+1) v rest
        inVal v v' = v == v'    
        inExp v exp = case exp of
            SingleValue v' -> inVal v v'
            Add v' v'' -> inVal v v' || inVal v v''
            Sub v' v'' -> inVal v v' || inVal v v''
            Mul v' v'' -> inVal v v' || inVal v v''
            Div v' v'' -> inVal v v' || inVal v v''
            Mod v' v'' -> inVal v v' || inVal v v''
        inCond v (Cond v' _ v'') = inVal v v' || inVal v v''
        isId (Var id) id' = id == id'
        isId _ _ = False

whenIdOverwritten :: [Command] -> Identifier  -> Integer
whenIdOverwritten commands id = go 0 id commands
    where
        go 5 _ _ = 5
        go _ _ [] = 10
        go n i (Assign id exp:rest) 
            | i == id = n
            | otherwise = go (n+1) i rest
        go n i (IfElse cond ins ins':rest) = min (go (n+1) i (ins++rest)) (go (n+1) i (ins'++rest))
        go n i (If cond ins:rest)= min (go (n+1) i ins) (go (n+1) i rest)
        go n i (While cond ins:rest) = go (n+1) i ins
        go n i (Until cond ins:rest) = go (n+1) i ins
        go n i (ForUp str _ _ ins:rest)
            | SingleVar str == i = n
            | otherwise = go (n+1) i ins
        go n i (ForDown str _ _ ins:rest)
            | SingleVar str == i = n
            | otherwise = go (n+1) i ins
        go n i (Read id:rest)
            |  i == id = n 
            | otherwise = go (n+1) i rest
        go n i (Write _:rest) = go (n+1) i rest

findFittingConst :: Value -> [(Register, [Content])] -> Maybe Register
findFittingConst (Const c) list = case prepared of
    [] -> Nothing
    (l:ls) -> Just $ first $ List.foldl' compSecond l ls
    where
        maped = fmap (Data.Bifunctor.second (fmap costToGenerate . foldConst)) list
        prepared = deleteMaybe $ fmap takeMaybe maped
        compSecond :: (Ord b) => (a,b) -> (a,b) -> (a,b)
        compSecond t1@(_,a) t2@(_,b)
            | a > b = t1
            | otherwise = t2
        foldConst :: [Content] -> Maybe Integer
        foldConst [] = Nothing
        foldConst (Constant int:_) = Just int
        foldConst (_:rest) = foldConst rest
        costToGenerate :: Integer -> Integer
        costToGenerate int = computeCosts (bestGenerateConst int c A)
        first (x, _) = x
findFittingConst _ _ = Nothing

findFittingRegister :: Value -> [(Register, [Content])] -> Maybe Register
findFittingRegister target list = fmap first (maybeHead $ Prelude.filter (\(_, x) -> isAny (sameId target) x) list)    
    where
        isAny _ [] = False
        isAny f (x:xs) = f x || isAny f xs
        sameId :: Value -> Content -> Bool
        sameId (Var id) (Variable id' _) = id == id'
        sameId _ _ = False
        first (a, _) = a

unsavedValueToContent :: Value -> Content
unsavedValueToContent (Const c) = Constant c
unsavedValueToContent (Var id) = Variable id False

myMax :: (a -> a -> Bool) -> [a] -> Maybe a
myMax _ [] = Nothing
myMax f (x:xs) = Just $ foldl (\p c -> if f p c then p else c) x xs

maybeMax :: (Ord a) => Maybe a -> Maybe a -> Bool
maybeMax Nothing _ = False
maybeMax _ Nothing = True
maybeMax (Just a) (Just b) = a > b