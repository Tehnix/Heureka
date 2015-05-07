{-# LANGUAGE OverloadedStrings #-}
module ReadCSV (
      parseCityMapCSV
    , parseInferenceCSV
    ) where
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char                  (ord)
import           Data.Vector
import           Data.Csv
import           Prelude                    hiding (map)


-- | Set the delimiter to use spaces
customOptions = defaultDecodeOptions {
      decDelimiter = fromIntegral (ord ' ')
    }

-- | Parse the CSV file or fail with an error
parseCSV filename transformResult decoder = do
    f <- readFile filename
    case decoder (C.pack f) of
        Left err -> fail err
        Right res -> return $ transformResult res

-- | Parse the structure of the citymap CSV
parseCityMapCSV :: FilePath -> IO [(Int, Int, String, Int, Int)]
parseCityMapCSV fname = parseCSV fname toList $ decodeWith customOptions NoHeader

-- | Parse the structure of the inference CSV
parseInferenceCSV :: FilePath -> IO [[String]]
parseInferenceCSV fname = parseCSV fname (toList.map toList) $ decodeWith customOptions NoHeader
