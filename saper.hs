
import Text.Read
import Data.Maybe
import System.Random
import Numeric
import Data.List
import Data.List.Split

-- NOT GAME:

_1thOf3 (a, _, _) = a
_2thOf3 (_, a, _) = a
_3thOf3 (_, _, a) = a

getMaybeInt :: IO (Maybe Int)
getMaybeInt = getLine >>= (return . readMaybe)

makeDichoList majorSize minorSize = replicate minorSize 1 ++ replicate (majorSize - minorSize) 0

sortBySnd (a1, b1) (a2, b2)
  | b1 > b2 = GT
  | b1 < b2 = LT
  | otherwise = EQ

twistList origin = do
    comparator <- sequence $ replicate (length origin) (randomIO :: IO Float)
    let weighted = zip origin comparator
    let sorted = sortBy sortBySnd weighted
    let unpacked = map (fst) sorted
    return unpacked

showOut x = (putStrLn . show) x
showOutLn x = (putStrLn . show) x

updListAt idx val lst = map repl $ zip lst [0 .. ]
    where
        repl (oldVal, eachIdx)
            | eachIdx == idx = val
            | otherwise = oldVal

insertBetween index value list = (take index list) ++ [value] ++ (drop index list)

-- GAME:

data SapperCell = SapperCell {
        scIsOpened :: Bool
        , scIsMined :: Bool
    } deriving (Show)

data SapperField = SapperField {
        sfWidth :: Int
        , sfHeight :: Int
        , sfData :: [SapperCell]
        , sfFlags :: [Bool]
        , sfPtrX :: Int
        , sfPtrY :: Int
    } deriving (Show)

safeFirstMove field sx sy = SapperField width height newCells [] ptrX ptrY
    where        
        cells = sfData field
        width = sfWidth field
        height = sfHeight field
        ptrX = sfPtrX field
        ptrY = sfPtrY field
        accessor = (sx + sy * width)
        newCells = insertBetween accessor (SapperCell False False) cells

safifyAt field sx sy = if (length cells) == width * height then field else safeFirstMove field sx sy
    where        
        cells = sfData field
        width = sfWidth field
        height = sfHeight field

isOutsiderAt field sx sy = (sx < 0) || (sx > width - 1) || (sy < 0) || (sy > height - 1)
    where
        width = sfWidth field
        height = sfHeight field

isWtfAt wtf dummy field sx sy = if outsider
        then dummy
        else if insider
            then 1
            else 0
    where
        cells = sfData field
        width = sfWidth field
        height = sfHeight field
        outsider = isOutsiderAt field sx sy
        insider = wtf $ cells !! (sx + sy * width)

isMinedAt = isWtfAt scIsMined 0
isOpenedAt = isWtfAt scIsOpened 1
isPtrAt field sx sy = (sfPtrX field) == sx && (sfPtrY field) == sy

setOpened opened cell = SapperCell (opened) (scIsMined cell)

updFieldAt field mapper sx sy = SapperField width height newCells [] ptrX ptrY
    where        
        cells = sfData field
        width = sfWidth field
        height = sfHeight field
        ptrX = sfPtrX field
        ptrY = sfPtrY field
        accessor = (sx + sy * width)
        newCell = mapper (cells !! accessor)
        newCells = updListAt accessor newCell cells

surrScanMask = [(x, y) | x <- iter, y <- iter] \\ [(0, 0)]
    where
        iter = [-1 .. 1]

surroundingsAt field sx sy = foldl (+) 0 (map (\mask -> isMinedAt field (sx + fst mask) (sy + snd mask)) surrScanMask)

-- TODO: optimize in a functional way
recursifyField field sx sy = l8
    where
        l0 = field
        l1 = openFieldDeepAt l0 (sx - 1) (sy - 1)
        l2 = openFieldDeepAt l1 (sx    ) (sy - 1)
        l3 = openFieldDeepAt l2 (sx + 1) (sy - 1)
        l4 = openFieldDeepAt l3 (sx - 1) (sy    )
        l5 = openFieldDeepAt l4 (sx + 1) (sy    )
        l6 = openFieldDeepAt l5 (sx - 1) (sy + 1)        
        l7 = openFieldDeepAt l6 (sx    ) (sy + 1)        
        l8 = openFieldDeepAt l7 (sx + 1) (sy + 1)

openFieldDeepAt field sx sy = newField
    where
        isOpened = isOpenedAt field sx sy
        isMined = isMinedAt field sx sy
        isOutsider = isOutsiderAt field sx sy
        surrs = surroundingsAt field sx sy
        newShallowField = updFieldAt field (setOpened True) sx sy
        updatedField = recursifyField newShallowField sx sy
        newField = if isOutsider || (isMined == 1) || (isOpened == 1) || (surrs > 0)
            then newShallowField
            else updatedField
        

openFieldAtPtr field = openFieldDeepAt safeField ptrX ptrY
    where 
        safeField = safifyAt field ptrX ptrY
        ptrX = sfPtrX field
        ptrY = sfPtrY field

movePtr dx dy field = SapperField width height cells [] newX newY
    where
        cells = sfData field
        width = sfWidth field
        height = sfHeight field
        ptrX = sfPtrX field
        ptrY = sfPtrY field
        newX = mod (ptrX + dx) width
        newY = mod (ptrY + dy) height

doByPrompt field prompt
    | prompt == "u" = movePtr 0 (-1) field
    | prompt == "d" = movePtr 0 1 field
    | prompt == "l" = movePtr (-1) 0 field
    | prompt == "r" = movePtr 1 0 field
    | prompt == "x" = openFieldAtPtr field
    | otherwise = field

drawAtCellByField field sx sy str = [sel !! 0] ++ str ++ [sel !! 1]
    where
        sel = if isPtrAt field sx sy then "[]" else "  "

drawEnumCellBySurr textSurr drawAtCell (cell, x, y) = drawAtCell x y $ if (not $ scIsOpened cell) then "~" else if (scIsMined cell) then "@" else (textSurr x y)

drawField field = drawField allRowSplit
    where
        preCells = sfData field
        width = sfWidth field
        height = sfHeight field
        size = width * height
        cells = if (length preCells) == size then preCells else replicate size (SapperCell False False);
        enumY = [0 .. height - 1] >>= replicate width
        enumX = take size (cycle [0 .. width - 1])
        all = zip3 cells enumX enumY
        allRowSplit = chunksOf width all
        surr = surroundingsAt field        
        textSurr sx sy = if (srNum == 0) then "." else show srNum where srNum = surr sx sy
        drawAtCell = drawAtCellByField field
        drawEnumCell = drawEnumCellBySurr textSurr drawAtCell
        drawRow row = "=|" ++ (foldr (++) "" (map drawEnumCell row)) ++ "|=\n"
        horFrame = (replicate (width * 3 + 4) '=') ++ "\n"
        drawField fld = horFrame ++ (foldr (++) "" (map drawRow fld)) ++ horFrame

-- Repeats input until a valid integer is provided
repPromptInt first onError rePrompt onSuccess = do
    putStrLn first
    fValueMb <- getMaybeInt
    if (isNothing fValueMb)
    then do
        putStrLn onError
        repPromptInt rePrompt onError rePrompt onSuccess
    else do
        putStrLn onSuccess
        return $ fromMaybe 0 fValueMb

gameLoopPlaying field = do
    putStrLn $ drawField field
    prompt <- getLine
    putStrLn ""
    let newField = doByPrompt field prompt
    gameLoopPlaying newField

gameLoopMain = do
    fWidth <- return 16 -- repPromptInt "Enter field width:" "Wrong value" "Enter field width again:" "Width set\n"
    fHeight <- return 8 -- repPromptInt "Enter field height:" "Wrong value" "Enter field height again:" "Height set\n"
    fMines <- return 20 --repPromptInt "Enter mines count:" "Wrong value" "Enter mines count again:" "Mines set\n"
    let fSize = fWidth * fHeight
    listMask <- twistList $ makeDichoList (fSize - 1) fMines -- We need the first list to be 1 step shorter as we insert a safe field
    let initialScList = (map (\x -> SapperCell False (x == 1)) listMask) -- ++ [SapperCell False False]
    let sapperField = SapperField fWidth fHeight initialScList [] 4 4
    gameLoopPlaying sapperField

main = gameLoopMain
























































