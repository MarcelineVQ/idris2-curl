module Network.Curl.Prim

import Network.Curl.Types

import public Network.Curl.Prim.Easy
import public Network.Curl.Prim.Global
import public Network.Curl.Prim.Mime
import public Network.Curl.Prim.Multi
import public Network.Curl.Prim.Other

import Data.IORef

import Data.List as List

import Derive.Prim

import Util

-- every init must have a cleanup, but we can re-use things before cleanup too,
-- so allow for this
-- e.g. 'If you want to re-use an easy handle that was added to the multi handle
-- for transfer, you must first remove it from the multi stack and then re-add
-- it again'

-- all curl has to start with global init and and with global cleanup
-- each 'easy' has to then start with easy init and end with easy cleanup
-- 'multi' is many easy with a multiperform instead of easy perform
-- How do I enforce that multi eats all easy in scope?
-- Can I simply curl_multi_add_handle with a linear easy to eat it up?
-- runCurl : IO ()
-- runCurl = do
--   CURLE_OK <- curl_global_init
--     | r => putStrLn $ "global init not ok: " ++ show r
--   Just h <- curl_easy_init
--     | Nothing => putStrLn "dsf"
--   CURLE_OK <- curl_easy_setopt h CURLOPT_URL "http://example.com"
--     | r => putStrLn $ "setopt not ok: " ++ show r
--   Right b <- pure $ the (Either Char (List Bits8)) $ Right [1,2,3] -- easy_to_bytes h
--     | Left r => putStrLn $ "tobytes not ok: " ++ show r
--   -- printLn b
--   printLn $ foldr (\b,ss => strCons (cast (cast {to=Int} b)) ss) "" b
--   CURLE_OK <- curl_easy_perform h
--     | r => putStrLn $ "perform not ok: " ++ show r
--   curl_easy_cleanup h
-- 
  -- globalinit
  -- easyinit
  -- set url
  -- set where/how to save
  -- perform
  -- easycleanup
  -- globalcleanup
  -- ?runCurl_rhs

-- It would be a good idea to query for all the settings on global init so we
-- can save and restore them for certain operations, sort of a ReaderT 'local'
-- kind of thing maybe



