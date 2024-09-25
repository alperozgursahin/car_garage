{-# LANGUAGE ImportQualifiedPost #-}

module Lib2
    ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition,
      Car(..)        -- Car data type exporting
    ) where

-- | An entity which represents user input.
-- It defines different types of commands that can be parsed.
data Query
    = Add Car Query    -- Recursive Add command with a single car and a continuation
    | Remove Int       -- Remove command with an id
    | List             -- List command
    | Quit             -- Quit command
    deriving (Eq, Show)

-- | A car data type to represent a car's details.
data Car = Car
    { make  :: String   -- The make of the car
    , model :: String   -- The model of the car
    , year  :: Int      -- The year of manufacture
    } deriving (Eq, Show)

-- | Parses user's input.
-- It takes a string input and converts it into a Query type.
parseQuery :: String -> Either String Query
parseQuery input =
    case words input of
        ("add":rest) -> parseAdd (unwords rest)  -- Parse add command with remaining string
        ["list"]     -> Right List                -- Parse list command
        ["remove", n] | all (`elem` "0123456789") n -> Right (Remove (read n))  -- Parse remove command
        ["quit"]     -> Right Quit                -- Parse quit command
        _            -> Left "Unknown command"    -- Handle unknown commands

-- Helper function to parse add command.
-- It processes a string input and splits it into individual car entries.
parseAdd :: String -> Either String Query
parseAdd [] = Left "No cars to add."            -- Handle empty input
parseAdd carsInput =
    let cars = splitByComma carsInput
    in if null cars
       then Left "No cars to add."                -- Handle case with no cars
       else parseCars cars                         -- Parse the list of cars

-- Custom function to split input string by comma.
-- This function splits the input based on commas.
splitByComma :: String -> [String]
splitByComma [] = []
splitByComma xs = let (first, rest) = break (== ',') xs
                  in first : case rest of
                                [] -> []
                                _:ys -> splitByComma (dropWhile (== ' ') ys)

-- Recursive function to parse multiple cars.
-- This function processes a list of car strings and converts them into Add commands.
parseCars :: [String] -> Either String Query
parseCars [] = Left "No cars to add."          -- Handle case with no cars
parseCars [carStr] = Right $ Add (parseCar carStr) Quit -- End of recursion
parseCars (carStr:rest) = do
    let nextQuery = parseCars rest               -- Recursively parse the rest of the cars
    case nextQuery of
        Left err -> Left err                      -- Propagate errors
        Right q  -> Right $ Add (parseCar carStr) q

-- Helper to parse a car (make, model, year).
-- It converts a string representation of a car into a Car data type.
parseCar :: String -> Car
parseCar carStr =
    let parts = words carStr
    in case parts of
        [make, model, yearStr] -> Car make model (read yearStr)  -- Construct Car instance
        _                      -> error "Invalid car format"       -- Handle invalid format

-- | An entity which represents your program's state.
-- It contains the list of cars currently stored in the program.
data State = State
    { cars :: [Car]   -- List of cars in the garage
    } deriving (Eq, Show)

-- | Creates an initial program's state.
-- This function initializes the state with an empty list of cars.
emptyState :: State
emptyState = State { cars = [] }

-- | Updates a state according to a query.
-- This function takes the current state and a user command and updates the state accordingly.
-- Right contains an optional message to print and an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state (Add newCar nextQuery) =
    let newState = state { cars = cars state ++ [newCar] }  -- Add new car to the list
    in case nextQuery of
        Quit -> Right (Just "Cars added.", newState)         -- If Quit, return message
        _    -> stateTransition newState nextQuery           -- Continue processing next queries

-- Handle List command.
stateTransition state List =
    if null (cars state)
    then Right (Just "No cars available.", state)          -- Return message if no cars
    else 
        let carList = unlines $ zipWith (\i car -> show i ++ ": " ++ show car) [1..] (cars state)
            trimmedCarList = if null carList then "" else init carList  -- Remove trailing newline
        in Right (Just trimmedCarList, state)               -- Return the list of cars

-- Handle Remove command.
stateTransition state (Remove idx) =
    if idx > 0 && idx <= length (cars state)
    then
        let newState = state { cars = take (idx - 1) (cars state) ++ drop idx (cars state) }
        in Right (Just "Car removed.", newState)            -- Car successfully removed
    else 
        Right (Just "Invalid car ID.", state)              -- Return error for invalid ID

-- Handle Quit command.
stateTransition state Quit =
    Right (Just "Goodbye!", state)                         -- Return goodbye message

-- | Helper function to split list into chunks of size n.
-- This function splits a list into smaller lists of a specified size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
