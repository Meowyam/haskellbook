-- http://www.serpentine.com/wreq/tutorial.html

module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

-- replace with other websites
-- if desired or needed
urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

-- gives list of io actions
mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

-- io action with list of responses
traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls




