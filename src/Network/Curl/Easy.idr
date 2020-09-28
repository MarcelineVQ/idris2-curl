module Network.Curl.Easy

import public Network.Curl.Prim.Global
import public Network.Curl.Types

-- The 'Easy Curl Interface'
{-
The easy interface is a synchronous, efficient, quickly used and... yes, easy
interface for file transfers. Numerous applications have been built using this.

When using libcurl you init your easy-session and get a handle, which you use as
input to the following interface functions you use.

You continue by setting all the options you want in the upcoming transfer, most
important among them is the URL itself. You might want to set some callbacks as
well that will be called from the library when data is available etc.

When all is setup, you tell libcurl to perform the transfer. It will then do the
entire operation and won't return until it is done or failed.

After the performance is made, you may get information about the transfer and
then you cleanup the easy-session's handle and libcurl is entirely off the hook!

-}

-- withEasy = init *> do <* cleanup

import Network.Curl.Types
import Network.Curl.Prim


export
easyFetchUrl : HasIO io => (url : String) -> io String
easyFetchUrl = ?dsdfs

export
curlEasyInit : HasIO io => io (Maybe (CurlHandle Easy))
curlEasyInit = curl_easy_init

export
curlEasyCleanup : HasIO io => CurlHandle Easy -> io ()
curlEasyCleanup = curl_easy_cleanup

export
curlEasySetopt : HasIO io => CurlHandle Easy -> {ty : _}
              -> (opt : CurlEOption ty) -> paramType opt -> io CurlECode
curlEasySetopt = curl_easy_setopt

export
curlEasyPerform : HasIO io => CurlHandle Easy -> io CurlECode
curlEasyPerform = curl_easy_perform
