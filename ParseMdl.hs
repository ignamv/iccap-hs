{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ParseMdl
( varTable
) where


import Text.ParserCombinators.Parsec
import Data.Complex
import Data.Either (lefts)
import Text.Parsec.Error
import Control.Applicative (liftA2)
import Control.Monad (liftM2)

import GHC.Generics (Generic)
import Control.DeepSeq

quotedField :: GenParser Char st String
quotedField = many $ noneOf "\""

type IccapDataset = [Complex Double]
data Variable = Variable { varName :: String,
                           varValue :: String }
                           deriving (Eq, Show, Generic, NFData)

data Table = Table { tableName :: String,
                     tableVars :: [Variable]}
                     deriving (Generic, NFData)
type HypTable = Table
type InstrumentOptionTable = Table

instance Show Table where
    show (Table name _) = "HypTable " ++ (show name) ++ " .."

data IccapDatasetGroup = Measured { getMeasured :: IccapDataset}
                       | Simulated { getSimulated :: IccapDataset}
                       | Common { getCommon :: IccapDataset }
                       | MeasuredSimulated { getMeasured :: IccapDataset, 
                                             getSimulated :: IccapDataset}
                     deriving (Generic, NFData)

instance Show IccapDatasetGroup where
    show (Measured xs) = "Measured [" ++ (show . length) xs ++ " points]"
    show (Simulated xs) = "Simulated [" ++ (show . length) xs ++ " points]"
    show (Common xs) = "Common [" ++ (show . length) xs ++ " points]"
    show (MeasuredSimulated xs _) = "MeasuredSimulated [" ++ (show . length) xs ++ " points]"

data Sweep = Sweep { swpName :: String,
                     swpTables :: [HypTable] } 
                     deriving (Generic, NFData)
instance Show Sweep where
    show (Sweep name _) = "Sweep " ++ (show name) ++ " .."

data Output = Output { outName :: String,
                       outTables :: [HypTable],
                       outDataset :: IccapDatasetGroup } 
                     deriving (Generic, NFData)

data Plot = Plot { pltName :: String } 
                     deriving (Show, Generic, NFData)

instance Show Output where
    show (Output name _ _) = "Output " ++ (show name) ++ " .."

type Transform = Output

data Setup = Setup { setName :: String,
                     setVars :: [Variable],
                     setSweeps :: [Sweep],
                     setOutputs :: [Output],
                     setTransforms :: [Transform] }
                     deriving (Generic, NFData)
instance Show Setup where
    show (Setup name _ sweeps outputs transforms) = ("Setup " 
        ++ (show name) ++ " .. " ++ (show sweeps) ++ " " 
        ++ (show outputs) ++ " " ++ (show transforms))

data Dut = Dut { dutName :: String,
                 dutVars :: [Variable],
                 dutSetups :: [Setup] }
                     deriving (Generic, NFData)
instance Show Dut where
    show (Dut name _ setups) = ("Dut " 
        ++ (show name) ++ " .. " ++ (show setups))

data Model = Model { mdlName :: String,
                 mdlVars :: [Variable],
                 mdlDuts :: [Dut] }
                     deriving (Generic, NFData)
instance Show Model where
    show (Model name _ duts) = ("Model " 
        ++ (show name) ++ " .. " ++ (show duts))

skipBlock :: GenParser Char st ()
skipBlock = do
    startBlock
    skipBlock' 0

startBlock = string "{\n"
endBlock = string "}\n"
notaBlock = try (string "\n") <|> noneOf "{}" <:> line

skipBlock' :: Int -> GenParser Char st ()
skipBlock' (-1) = return ()
skipBlock' nesting = (
    (startBlock >> skipBlock' (nesting+1)) <|>
    (endBlock >> skipBlock' (nesting-1)) <|>
    (notaBlock >> skipBlock' nesting))

-- element 0 "Name" "TimeMeasured"
varTableLine :: String -> GenParser Char st String
varTableLine field1 = do
    try (string "element " >> many digit >> (string $ " \"" ++ field1 ++ "\" \""))
    field2 <- quotedField
    string "\"\n"
    return field2

-- element "Unit" "SMU1"
tableVar :: GenParser Char st Variable
tableVar = do
    string "element \""
    name <- quotedField
    string "\" \""
    value <- quotedField
    string "\"\n"
    return $ Variable name value

-- element 0 "Use User Sweep" "No"
instrVar :: GenParser Char st Variable
instrVar = do
    string "element 0 \""
    name <- quotedField
    string "\" \""
    value <- quotedField
    string "\"\n"
    return $ Variable name value

varTableVar :: GenParser Char st Variable
varTableVar = (Variable <$> varTableLine "Name" <*> varTableLine "Value") <* (optional $ varTableLine "Comment")

varTable :: String -> GenParser Char st [Variable]
varTable name = do
    try . string $ "TABLE \"" ++ name ++ "\""
    versionField
    newline
    startBlock
    many viewtok
    vars <- many varTableVar
    endBlock
    if last vars == Variable "" ""
       then return $ init vars
       else fail "Expected empty name/value at end of variable table"

(<++>) :: Applicative f => f [b] -> f [b] -> f [b]
(<++>) = liftA2 (++)
infixr 5 <++>
(<:>) :: Applicative f => f b -> f [b] -> f [b]
(<:>) = liftA2 (:) 
infixr 5 <:>

once :: Functor f => f a -> f [a]
once = fmap pure

line :: GenParser Char st String
line = manyTill anyChar newline

zeroOrOnce :: GenParser a st b -> GenParser a st [b]
zeroOrOnce = option [] . once

options :: [String] -> GenParser Char st String
options = foldr1 (<|>) . map string

iccapFloat :: GenParser Char st Double
iccapFloat = read <$> (sign <++> mantissa <++> exponent)
    where sign = zeroOrOnce (char '-')
          mantissa = many digit <++> (option "" $ char '.' <:> many digit)
          exponent = option "" $ oneOf "eE" <:> (zeroOrOnce $ oneOf "+-") <++> many digit

-- point 6 1 1 4.58561E-015 0
dataPoint :: GenParser Char st (Complex Double)
dataPoint = do
    string "point "
    many digit -- TODO: check sequence
    string " 1 1 "
    real <- iccapFloat
    char ' '
    imag <- iccapFloat
    newline
    return $ real :+ imag

dataset :: GenParser Char st IccapDatasetGroup
dataset = do
    string "type "
    dtype <- options ["MEAS", "SIMU", "COMMON"]
    newline
    points <- many dataPoint
    let groupType = case dtype of "MEAS" -> Measured
                                  "SIMU" -> Simulated
                                  "COMMON" -> Common
    return $ groupType points

combineDatasets :: IccapDatasetGroup -> IccapDatasetGroup -> IccapDatasetGroup
combineDatasets (Measured p1) (Simulated p2) = MeasuredSimulated p1 p2
combineDatasets (Simulated p2) (Measured p1) = MeasuredSimulated p1 p2
combineDatasets _ _ = error "Can not combine Dataset groups"

datasize :: GenParser Char st ()
datasize = do
    try $ string "datasize "
    options ["MEAS", "SIMU", "COMMON", "BOTH"]
    char ' '
    many digit
    string " 1 1\n"
    return ()

datasets :: GenParser Char st IccapDatasetGroup
datasets = do
    string "dataset\n"
    startBlock
    optional datasize
    (foldr1 combineDatasets <$> many1 dataset) <* endBlock

hyptable :: GenParser Char st HypTable
hyptable = do
    string "HYPTABLE \""
    name <- quotedField
    string "\"\n"
    startBlock
    many viewtok
    vars <- many tableVar
    endBlock
    return $ Table name vars

versionField :: GenParser Char st ()
versionField = optional $ string " \"" >> quotedField >> char '\"'

linkHeader :: String -> GenParser Char st String
linkHeader linkType = do
    try . string $ "LINK " ++ linkType 
    string " \""
    name <- quotedField
    char '"'
    versionField
    newline
    startBlock
    many applictok
    many subapptok
    many viewtok
    return name

sweep :: GenParser Char st Sweep
sweep = do
    name <- linkHeader "SWEEP"
    startDataBlock
    tables <- many hyptable
    endBlock
    many listtok
    endBlock
    return $ Sweep name tables

testsweep = parseFromFile sweep "sweep.inp"

output :: GenParser Char st Output
output = do
    name <- linkHeader "OUT"
    startDataBlock
    tables <- many hyptable
    ds <- datasets
    endBlock
    many listtok
    endBlock
    return $ Output name tables ds

testoutput = parseFromFile output "output.out"

instrumentoptions :: GenParser Char st InstrumentOptionTable
instrumentoptions = do
    string "TABLE \""
    name <- quotedField
    char '"'
    versionField
    newline
    startBlock
    many viewtok
    vars <- many instrVar
    endBlock
    return $ Table name vars

testinstrumentoptions = parseFromFile instrumentoptions "instrumentoptions.iot"

setup :: GenParser Char st Setup
setup = do
    name <- linkHeader "DAT"
    vars <- varTable "Variable Table"
    optional $ varTable "GUI Table"
    sweeps <- many sweep
    outputs <- many output
    transforms <- many transform
    many plot
    startDataBlock
    instroptions <- many instrumentoptions
    endBlock
    endBlock
    return $ Setup name vars sweeps outputs transforms

testsetup = parseFromFile setup "setup2.set"

ignoreToken :: String -> GenParser Char st ()
ignoreToken tok = do
    try $ string tok
    x <- line
    if x == "" || head x == ' ' then return () else fail $ "Expecting token " ++ tok

viewtok = ignoreToken "View"
membertok = ignoreToken "member"
listtok = ignoreToken "list"
applictok = ignoreToken "applic"
subapptok = ignoreToken "subapp"
editsizetok = ignoreToken "editsize"

ignoreBlock :: String -> GenParser Char st ()
ignoreBlock tok = ignoreToken tok >> skipBlock

ignoreLink linkType = linkHeader linkType >> skipBlock' 0

optimedit = ignoreBlock "OPTIMEDIT"

plot :: GenParser Char st Plot
plot = do
    name <- linkHeader "PLOT"
    skipBlock' 0
    return $ Plot name

testplot = parseFromFile plot "plot.plt"

escapedLine = string "\n" <|> (char ' ' >> line)

blockedit :: GenParser Char st ()
blockedit = do
    string "BLKEDIT \"Program Body\"\n"
    startBlock
    (newline >> return ()) <|> ((many escapedLine) >> return ())
    endBlock
    return ()

startDataBlock = do
    string "data"
    versionField
    newline
    startBlock
    many editsizetok

transform :: GenParser Char st Transform
transform = do
    name <- linkHeader "XFORM"
    startDataBlock
    tables <- many hyptable
    option () optimedit
    many blockedit
    ds <- datasets
    endBlock
    many listtok
    many membertok
    endBlock
    return $ Output name tables ds

testtransform = parseFromFile transform "transform.xfm"

dut :: GenParser Char st Dut
dut = do
    name <- linkHeader "DUT"
    vars <- varTable "Variable Table"
    optional $ varTable "GUI Table"
    ignoreLink "TCIRC"
    ignoreLink "DPS"
    ignoreLink "CONN"
    setups <- many setup
    return $ Dut name vars setups

testdut = parseFromFile dut "dut.dut"

plotOptions :: GenParser Char st ()
plotOptions = do
    string "TABLE \"Plot Options\""
    versionField
    newline
    skipBlock

model :: GenParser Char st Model
model = do
    name <- linkHeader "MODEL"
    vars <- varTable "Variable Table"
    optional $ varTable "GUI Table"
    optional $ plotOptions
    ignoreLink "CIRC"
    ignoreLink "PS"
    many $ ignoreLink "MACRO"
    duts <- many dut
    return $ Model name vars duts

parseFromFile2 parser filename = do
    contents <- filter (/= '\r') <$> readFile filename
    return $ parse parser filename contents

testmodel = parseFromFile2 model "model.mdl"

testmodel2 = do
    mdls <- lines <$> readFile "models.txt"
    errors <- lefts <$> mapM (parseFromFile2 model) mdls
    let formatted = unlines . concat $ map quickfixError errors
    writeFile "parse_errors.txt" formatted

quickfixError :: ParseError -> [String]
quickfixError err = let pos = errorPos err
                        line = sourceLine pos
                        col = sourceColumn pos
                        file = sourceName pos
                        msgs = errorMessages err
                    in [file ++ ":" ++ show line ++ ":" ++ show col ++ " " ++ messageString msg | msg <- msgs]
