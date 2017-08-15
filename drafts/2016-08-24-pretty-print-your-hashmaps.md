    {-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
    import qualified Prelude as P
    import qualified Data.ByteString.Lazy.Char8 as C
    import qualified Data.Aeson.Encode.Pretty as Pretty

    data DAG a
      = Node a [ DAG a ]
      | Leaf a deriving (Show, Eq, Generic, ToJSON, FromJSON)

    prettify = P.putStrLn . C.unpack . Pretty.encodePretty

in ghci:

    >>> prettify $ Leaf "test"

However, we need a better way to prettify things that are `fromList`able.

