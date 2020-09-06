module Network.Curl.Types.CurlCode

import  Network.Curl.Types.Code

import Util

import Derive.Enum
%language ElabReflection

public export
data CurlCode
  = CURLE_OK                      {- 0 -}
  | CURLE_UNSUPPORTED_PROTOCOL    {- 1 -}
  | CURLE_FAILED_INIT             {- 2 -}
  | CURLE_URL_MALFORMAT           {- 3 -}
  | CURLE_NOT_BUILT_IN            {- 4 - [was obsoleted in August 2007 for
                                    7.17.0, reused in April 2011 for 7.21.5] -}
  | CURLE_COULDNT_RESOLVE_PROXY   {- 5 -}
  | CURLE_COULDNT_RESOLVE_HOST    {- 6 -}
  | CURLE_COULDNT_CONNECT         {- 7 -}
  | CURLE_WEIRD_SERVER_REPLY      {- 8 -}
  | CURLE_REMOTE_ACCESS_DENIED    {- 9 a service was denied by the server
                                    due to lack of access - when login fails
                                    this is not returned. -}
  | CURLE_FTP_ACCEPT_FAILED       {- 10 - [was obsoleted in April 2006 for
                                    7.15.4, reused in Dec 2011 for 7.24.0]-}
  | CURLE_FTP_WEIRD_PASS_REPLY    {- 11 -}
  | CURLE_FTP_ACCEPT_TIMEOUT      {- 12 - timeout occurred accepting server
                                    [was obsoleted in August 2007 for 7.17.0,
                                    reused in Dec 2011 for 7.24.0]-}
  |CURLE_FTP_WEIRD_PASV_REPLY    {- 13 -}
  |CURLE_FTP_WEIRD_227_FORMAT    {- 14 -}
  |CURLE_FTP_CANT_GET_HOST       {- 15 -}
  |CURLE_HTTP2                   {- 16 - A problem in the http2 framing layer.
                                    [was obsoleted in August 2007 for 7.17.0,
                                    reused in July 2014 for 7.38.0] -}
  | CURLE_FTP_COULDNT_SET_TYPE    {- 17 -}
  | CURLE_PARTIAL_FILE            {- 18 -}
  | CURLE_FTP_COULDNT_RETR_FILE   {- 19 -}
  | CURLE_OBSOLETE20              {- 20 - NOT USED -}
  | CURLE_QUOTE_ERROR             {- 21 - quote command failure -}
  | CURLE_HTTP_RETURNED_ERROR     {- 22 -}
  | CURLE_WRITE_ERROR             {- 23 -}
  | CURLE_OBSOLETE24              {- 24 - NOT USED -}
  | CURLE_UPLOAD_FAILED           {- 25 - failed upload "command" -}
  | CURLE_READ_ERROR              {- 26 - couldn't open/read from file -}
  | CURLE_OUT_OF_MEMORY           {- 27 -}
  {- Note: CURLE_OUT_OF_MEMORY may sometimes indicate a conversion error
           instead of a memory allocation error if CURL_DOES_CONVERSIONS
           is defined
  -}
  | CURLE_OPERATION_TIMEDOUT      {- 28 - the timeout time was reached -}
  | CURLE_OBSOLETE29              {- 29 - NOT USED -}
  | CURLE_FTP_PORT_FAILED         {- 30 - FTP PORT operation failed -}
  | CURLE_FTP_COULDNT_USE_REST    {- 31 - the REST command failed -}
  | CURLE_OBSOLETE32              {- 32 - NOT USED -}
  | CURLE_RANGE_ERROR             {- 33 - RANGE "command" didn't work -}
  | CURLE_HTTP_POST_ERROR         {- 34 -}
  | CURLE_SSL_CONNECT_ERROR       {- 35 - wrong when connecting with SSL -}
  | CURLE_BAD_DOWNLOAD_RESUME     {- 36 - couldn't resume download -}
  | CURLE_FILE_COULDNT_READ_FILE  {- 37 -}
  | CURLE_LDAP_CANNOT_BIND        {- 38 -}
  | CURLE_LDAP_SEARCH_FAILED      {- 39 -}
  | CURLE_OBSOLETE40              {- 40 - NOT USED -}
  | CURLE_FUNCTION_NOT_FOUND      {- 41 - NOT USED starting with 7.53.0 -}
  | CURLE_ABORTED_BY_CALLBACK     {- 42 -}
  | CURLE_BAD_FUNCTION_ARGUMENT   {- 43 -}
  | CURLE_OBSOLETE44              {- 44 - NOT USED -}
  | CURLE_INTERFACE_FAILED        {- 45 - CURLOPT_INTERFACE failed -}
  | CURLE_OBSOLETE46              {- 46 - NOT USED -}
  | CURLE_TOO_MANY_REDIRECTS      {- 47 - catch endless re-direct loops -}
  | CURLE_UNKNOWN_OPTION          {- 48 - User specified an unknown option -}
  | CURLE_TELNET_OPTION_SYNTAX    {- 49 - Malformed telnet option -}
  | CURLE_OBSOLETE50              {- 50 - NOT USED -}
  | CURLE_OBSOLETE51              {- 51 - NOT USED -}
  | CURLE_GOT_NOTHING             {- 52 - when this is a specific error -}
  | CURLE_SSL_ENGINE_NOTFOUND     {- 53 - SSL crypto engine not found -}
  | CURLE_SSL_ENGINE_SETFAILED    {- 54 - can not set SSL crypto engine as
                                    default -}
  | CURLE_SEND_ERROR              {- 55 - failed sending network data -}
  | CURLE_RECV_ERROR              {- 56 - failure in receiving network data -}
  | CURLE_OBSOLETE57              {- 57 - NOT IN USE -}
  | CURLE_SSL_CERTPROBLEM         {- 58 - problem with the local certificate -}
  | CURLE_SSL_CIPHER              {- 59 - couldn't use specified cipher -}
  | CURLE_PEER_FAILED_VERIFICATION {- 60 - peer's certificate or fingerprint
                                     wasn't verified fine -}
  | CURLE_BAD_CONTENT_ENCODING    {- 61 - Unrecognized/bad encoding -}
  | CURLE_LDAP_INVALID_URL        {- 62 - Invalid LDAP URL -}
  | CURLE_FILESIZE_EXCEEDED       {- 63 - Maximum file size exceeded -}
  | CURLE_USE_SSL_FAILED          {- 64 - Requested FTP SSL level failed -}
  | CURLE_SEND_FAIL_REWIND        {- 65 - Sending the data requires a rewind
                                    that failed -}
  | CURLE_SSL_ENGINE_INITFAILED   {- 66 - failed to initialise ENGINE -}
  | CURLE_LOGIN_DENIED            {- 67 - user, password or similar was not
                                    accepted and we failed to login -}
  | CURLE_TFTP_NOTFOUND           {- 68 - file not found on server -}
  | CURLE_TFTP_PERM               {- 69 - permission problem on server -}
  | CURLE_REMOTE_DISK_FULL        {- 70 - out of disk space on server -}
  | CURLE_TFTP_ILLEGAL            {- 71 - Illegal TFTP operation -}
  | CURLE_TFTP_UNKNOWNID          {- 72 - Unknown transfer ID -}
  | CURLE_REMOTE_FILE_EXISTS      {- 73 - File already exists -}
  | CURLE_TFTP_NOSUCHUSER         {- 74 - No such user -}
  | CURLE_CONV_FAILED             {- 75 - conversion failed -}
  | CURLE_CONV_REQD               {- 76 - caller must register conversion
                                    callbacks using curl_easy_setopt options
                                    CURLOPT_CONV_FROM_NETWORK_FUNCTION,
                                    CURLOPT_CONV_TO_NETWORK_FUNCTION, and
                                    CURLOPT_CONV_FROM_UTF8_FUNCTION -}
  | CURLE_SSL_CACERT_BADFILE      {- 77 - could not load CACERT file, missing
                                    or wrong format -}
  | CURLE_REMOTE_FILE_NOT_FOUND   {- 78 - remote file not found -}
  | CURLE_SSH                     {- 79 - error from the SSH layer, somewhat
                                    generic so the error message will be of
                                    interest when this has happened -}

  | CURLE_SSL_SHUTDOWN_FAILED     {- 80 - Failed to shut down the SSL
                                    connection -}
  | CURLE_AGAIN                   {- 81 - socket is not ready for send/recv,
                                    wait till it's ready and try again (Added
                                    in 7.18.2) -}
  | CURLE_SSL_CRL_BADFILE         {- 82 - could not load CRL file, missing or
                                    wrong format (Added in 7.19.0) -}
  | CURLE_SSL_ISSUER_ERROR        {- 83 - Issuer check failed.  (Added in
                                    7.19.0) -}
  | CURLE_FTP_PRET_FAILED         {- 84 - a PRET command failed -}
  | CURLE_RTSP_CSEQ_ERROR         {- 85 - mismatch of RTSP CSeq numbers -}
  | CURLE_RTSP_SESSION_ERROR      {- 86 - mismatch of RTSP Session Ids -}
  | CURLE_FTP_BAD_FILE_LIST       {- 87 - unable to parse FTP file list -}
  | CURLE_CHUNK_FAILED            {- 88 - chunk callback reported error -}
  | CURLE_NO_CONNECTION_AVAILABLE {- 89 - No connection available, the
                                    session will be queued -}
  | CURLE_SSL_PINNEDPUBKEYNOTMATCH {- 90 - specified pinned public key did not
                                     match -}
  | CURLE_SSL_INVALIDCERTSTATUS   {- 91 - invalid certificate status -}
  | CURLE_HTTP2_STREAM            {- 92 - stream error in HTTP/2 framing layer
                                    -}
  | CURLE_RECURSIVE_API_CALL      {- 93 - an api function was called from
                                    inside a callback -}
  | CURLE_AUTH_ERROR              {- 94 - an authentication function returned an
                                    error -}
  | CURLE_HTTP3                   {- 95 - An HTTP/3 layer problem -}
  | CURL_LAST {- never use! -}

public export
data GlobalFlag : Type where
  CURL_GLOBAL_SSL : GlobalFlag -- (1<<0) /* no purpose since since 7.57.0 */
  CURL_GLOBAL_WIN32 : GlobalFlag -- (1<<1)
  CURL_GLOBAL_ALL : GlobalFlag -- (CURL_GLOBAL_SSL|CURL_GLOBAL_WIN32)
  CURL_GLOBAL_NOTHING : GlobalFlag -- 0
  CURL_GLOBAL_DEFAULT : GlobalFlag -- CURL_GLOBAL_ALL
  CURL_GLOBAL_ACK_EINTR : GlobalFlag -- (1<<2)

export
ToCode GlobalFlag where
  toCode CURL_GLOBAL_SSL = 1
  toCode CURL_GLOBAL_WIN32 = 2
  toCode CURL_GLOBAL_ALL = 3
  toCode CURL_GLOBAL_NOTHING = 0
  toCode CURL_GLOBAL_DEFAULT = 3
  toCode CURL_GLOBAL_ACK_EINTR = 4

-- export
-- FromCode GlobalFlag where
--   fromCode 1 = CURL_GLOBAL_SSL
--   fromCode 2 = CURL_GLOBAL_WIN32
--   fromCode 3 = CURL_GLOBAL_ALL
--   fromCode 0 = CURL_GLOBAL_NOTHING
--   fromCode 4 = CURL_GLOBAL_ACK_EINTR
--   fromCode x = lie_idris_crash $ "Global flag was not a valid flag: " ++ show x

export
Show CurlCode where
  show = showEnum

export
Eq CurlCode where
  (==) = eqEnum

export
Ord CurlCode where
  compare = compareEnum

export
ToCode CurlCode where
  toCode = enumTo [0..96]

export
FromCode CurlCode where -- bad, FromCode should really be Maybe Int
  unsafeFromCode = unsafeEnumFrom [0..96]
  fromCode = enumFrom [0..96]

  -- fromCode 0 = CURLE_OK
  -- fromCode 1 = CURLE_UNSUPPORTED_PROTOCOL
  -- fromCode 2 = CURLE_FAILED_INIT
  -- fromCode 3 = CURLE_URL_MALFORMAT
  -- fromCode 4 = CURLE_NOT_BUILT_IN
  -- fromCode 5 = CURLE_COULDNT_RESOLVE_PROXY
  -- fromCode 6 = CURLE_COULDNT_RESOLVE_HOST
  -- fromCode 7 = CURLE_COULDNT_CONNECT
  -- fromCode 8 = CURLE_WEIRD_SERVER_REPLY
  -- fromCode 9 = CURLE_REMOTE_ACCESS_DENIED
  -- fromCode 10 = CURLE_FTP_ACCEPT_FAILED
  -- fromCode 11 = CURLE_FTP_WEIRD_PASS_REPLY
  -- fromCode 12 = CURLE_FTP_ACCEPT_TIMEOUT
  -- fromCode 13 = CURLE_FTP_WEIRD_PASV_REPLY
  -- fromCode 14 = CURLE_FTP_WEIRD_227_FORMAT
  -- fromCode 15 = CURLE_FTP_CANT_GET_HOST
  -- fromCode 16 = CURLE_HTTP2
  -- fromCode 17 = CURLE_FTP_COULDNT_SET_TYPE
  -- fromCode 18 = CURLE_PARTIAL_FILE
  -- fromCode 19 = CURLE_FTP_COULDNT_RETR_FILE
  -- fromCode 20 = CURLE_OBSOLETE20
  -- fromCode 21 = CURLE_QUOTE_ERROR
  -- fromCode 22 = CURLE_HTTP_RETURNED_ERROR
  -- fromCode 23 = CURLE_WRITE_ERROR
  -- fromCode 24 = CURLE_OBSOLETE24
  -- fromCode 25 = CURLE_UPLOAD_FAILED
  -- fromCode 26 = CURLE_READ_ERROR
  -- fromCode 27 = CURLE_OUT_OF_MEMORY
  -- fromCode 28 = CURLE_OPERATION_TIMEDOUT
  -- fromCode 29 = CURLE_OBSOLETE29
  -- fromCode 30 = CURLE_FTP_PORT_FAILED
  -- fromCode 31 = CURLE_FTP_COULDNT_USE_REST
  -- fromCode 32 = CURLE_OBSOLETE32
  -- fromCode 33 = CURLE_RANGE_ERROR
  -- fromCode 34 = CURLE_HTTP_POST_ERROR
  -- fromCode 35 = CURLE_SSL_CONNECT_ERROR
  -- fromCode 36 = CURLE_BAD_DOWNLOAD_RESUME
  -- fromCode 37 = CURLE_FILE_COULDNT_READ_FILE
  -- fromCode 38 = CURLE_LDAP_CANNOT_BIND
  -- fromCode 39 = CURLE_LDAP_SEARCH_FAILED
  -- fromCode 40 = CURLE_OBSOLETE40
  -- fromCode 41 = CURLE_FUNCTION_NOT_FOUND
  -- fromCode 42 = CURLE_ABORTED_BY_CALLBACK
  -- fromCode 43 = CURLE_BAD_FUNCTION_ARGUMENT
  -- fromCode 44 = CURLE_OBSOLETE44
  -- fromCode 45 = CURLE_INTERFACE_FAILED
  -- fromCode 46 = CURLE_OBSOLETE46
  -- fromCode 47 = CURLE_TOO_MANY_REDIRECTS
  -- fromCode 48 = CURLE_UNKNOWN_OPTION
  -- fromCode 49 = CURLE_TELNET_OPTION_SYNTAX
  -- fromCode 50 = CURLE_OBSOLETE50
  -- fromCode 51 = CURLE_OBSOLETE51
  -- fromCode 52 = CURLE_GOT_NOTHING
  -- fromCode 53 = CURLE_SSL_ENGINE_NOTFOUND
  -- fromCode 54 = CURLE_SSL_ENGINE_SETFAILED
  -- fromCode 55 = CURLE_SEND_ERROR
  -- fromCode 56 = CURLE_RECV_ERROR
  -- fromCode 57 = CURLE_OBSOLETE57
  -- fromCode 58 = CURLE_SSL_CERTPROBLEM
  -- fromCode 59 = CURLE_SSL_CIPHER
  -- fromCode 60 = CURLE_PEER_FAILED_VERIFICATION
  -- fromCode 61 = CURLE_BAD_CONTENT_ENCODING
  -- fromCode 62 = CURLE_LDAP_INVALID_URL
  -- fromCode 63 = CURLE_FILESIZE_EXCEEDED
  -- fromCode 64 = CURLE_USE_SSL_FAILED
  -- fromCode 65 = CURLE_SEND_FAIL_REWIND
  -- fromCode 66 = CURLE_SSL_ENGINE_INITFAILED
  -- fromCode 67 = CURLE_LOGIN_DENIED
  -- fromCode 68 = CURLE_TFTP_NOTFOUND
  -- fromCode 69 = CURLE_TFTP_PERM
  -- fromCode 70 = CURLE_REMOTE_DISK_FULL
  -- fromCode 71 = CURLE_TFTP_ILLEGAL
  -- fromCode 72 = CURLE_TFTP_UNKNOWNID
  -- fromCode 73 = CURLE_REMOTE_FILE_EXISTS
  -- fromCode 74 = CURLE_TFTP_NOSUCHUSER
  -- fromCode 75 = CURLE_CONV_FAILED
  -- fromCode 76 = CURLE_CONV_REQD
  -- fromCode 77 = CURLE_SSL_CACERT_BADFILE
  -- fromCode 78 = CURLE_REMOTE_FILE_NOT_FOUND
  -- fromCode 79 = CURLE_SSH
  -- fromCode 80 = CURLE_SSL_SHUTDOWN_FAILED
  -- fromCode 81 = CURLE_AGAIN
  -- fromCode 82 = CURLE_SSL_CRL_BADFILE
  -- fromCode 83 = CURLE_SSL_ISSUER_ERROR
  -- fromCode 84 = CURLE_FTP_PRET_FAILED
  -- fromCode 85 = CURLE_RTSP_CSEQ_ERROR
  -- fromCode 86 = CURLE_RTSP_SESSION_ERROR
  -- fromCode 87 = CURLE_FTP_BAD_FILE_LIST
  -- fromCode 88 = CURLE_CHUNK_FAILED
  -- fromCode 89 = CURLE_NO_CONNECTION_AVAILABLE
  -- fromCode 90 = CURLE_SSL_PINNEDPUBKEYNOTMATCH
  -- fromCode 91 = CURLE_SSL_INVALIDCERTSTATUS
  -- fromCode 92 = CURLE_HTTP2_STREAM
  -- fromCode 93 = CURLE_RECURSIVE_API_CALL
  -- fromCode 94 = CURLE_AUTH_ERROR
  -- fromCode 95 = CURLE_HTTP3
  -- fromCode 96 = CURL_LAST
  -- fromCode x = lie_idris_crash $ "CURLcode was not a valid code: " ++ show x
  