module Network.Curl.Types.Option

import Network.Curl.Types.Code
import Network.Curl.Types.CurlCode

import Derive.Enum
%language ElabReflection

data OptTag = LongTag | FunPtrTag | ObjPtrTag | OffTTag

public export
data OptType
  = CURLOPTTYPE_LONG -- at east 32 bits
  | CURLOPTTYPE_FUNCTIONPOINT -- void*
  | CURLOPTTYPE_OBJECTPOINT
  | CURLOPTTYPE_STRINGPOINT -- char*
  | CURLOPTTYPE_SLISTPOINT
  | CURLOPTTYPE_OFF_T -- int64

export
ToCode OptType where
  toCode CURLOPTTYPE_LONG = 0
  toCode CURLOPTTYPE_OBJECTPOINT = 10000
  toCode CURLOPTTYPE_FUNCTIONPOINT = 20000
  toCode CURLOPTTYPE_OFF_T = 30000

  toCode CURLOPTTYPE_STRINGPOINT = 10000
  toCode CURLOPTTYPE_SLISTPOINT =  10000

public export
data CurlOption : OptType -> Type where
  {- This is the FILE * or void * the regular output should be written to. -}
  CURLOPT_WRITEDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- The full URL to get/put -}
  CURLOPT_URL : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Port number to connect to, if other than default. -}
  CURLOPT_PORT : CurlOption CURLOPTTYPE_LONG

  {- Name of proxy to use. -}
  CURLOPT_PROXY : CurlOption CURLOPTTYPE_STRINGPOINT

  {- "user:password;options" to use when fetching. -}
  CURLOPT_USERPWD : CurlOption CURLOPTTYPE_STRINGPOINT

  {- "user:password" to use with proxy. -}
  CURLOPT_PROXYUSERPWD : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Range to get, specified as an ASCII string. -}
  CURLOPT_RANGE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- not used -}

  {- Specified file stream to upload from (use as input): -}
  CURLOPT_READDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- Buffer to receive error messages in, must be at least CURL_ERROR_SIZE
   * bytes big. -}
  CURLOPT_ERRORBUFFER : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- Function that will be called to store the output (instead of fwrite). The
   * parameters will use fwrite() syntax, make sure to follow them. -}
  CURLOPT_WRITEFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Function that will be called to read the input (instead of fread). The
   * parameters will use fread() syntax, make sure to follow them. -}
  CURLOPT_READFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Time-out the read operation after this amount of seconds -}
  CURLOPT_TIMEOUT : CurlOption CURLOPTTYPE_LONG

  {- If the CURLOPT_INFILE is used, this can be used to inform libcurl about
   * how large the file being sent really is. That allows better error
   * checking and better verifies that the upload was successful. -1 means
   * unknown size.
   *
   * For large file support, there is also a _LARGE version of the key
   * which takes an CURLOPTTYPE_OFF_T type, allowing platforms with larger CURLOPTTYPE_OFF_T
   * sizes to handle larger files.  See below for INFILESIZE_LARGE.
   -}
  CURLOPT_INFILESIZE : CurlOption CURLOPTTYPE_LONG

  {- POST static input fields. -}
  CURLOPT_POSTFIELDS : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- Set the referrer page (needed by some CGIs) -}
  CURLOPT_REFERER : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set the FTP PORT string (interface name, named or numerical IP address)
     Use i.e '-' to use default address. -}
  CURLOPT_FTPPORT : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set the User-Agent string (examined by some CGIs) -}
  CURLOPT_USERAGENT : CurlOption CURLOPTTYPE_STRINGPOINT

  {- If the download receives less than "low speed limit" bytes/second
   * during "low speed time" seconds, the operations is aborted.
   * You could i.e if you have a pretty high speed connection, abort if
   * it is less than 2000 bytes/sec during 20 seconds.
   -}

  {- Set the "low speed limit" -}
  CURLOPT_LOW_SPEED_LIMIT : CurlOption CURLOPTTYPE_LONG

  {- Set the "low speed time" -}
  CURLOPT_LOW_SPEED_TIME : CurlOption CURLOPTTYPE_LONG

  {- Set the continuation offset.
   *
   * Note there is also a _LARGE version of this key which uses
   * CURLOPTTYPE_OFF_T types, allowing for large file offsets on platforms which
   * use larger-than-32-bit CURLOPTTYPE_OFF_T's.  Look below for RESUME_FROM_LARGE.
   -}
  CURLOPT_RESUME_FROM : CurlOption CURLOPTTYPE_LONG

  {- Set cookie in request: -}
  CURLOPT_COOKIE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- This points to a linked list of headers, struct curl_slist kind. This
     list is also used for RTSP (in spite of its name) -}
  CURLOPT_HTTPHEADER : CurlOption CURLOPTTYPE_SLISTPOINT

  {- This points to a linked list of post entries, struct curl_httppost -}
  CURLOPT_HTTPPOST : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- name of the file keeping your private SSL-certificate -}
  CURLOPT_SSLCERT : CurlOption CURLOPTTYPE_STRINGPOINT

  {- password for the SSL or SSH private key -}
  CURLOPT_KEYPASSWD : CurlOption CURLOPTTYPE_STRINGPOINT

  {- send TYPE parameter? -}
  CURLOPT_CRLF : CurlOption CURLOPTTYPE_LONG

  {- send linked-list of QUOTE commands -}
  CURLOPT_QUOTE : CurlOption CURLOPTTYPE_SLISTPOINT

  {- send FILE * or void * to store headers to, if you use a callback it
     is simply passed to the callback unmodified -}
  CURLOPT_HEADERDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- point to a file to read the initial cookies from, also enables
     "cookie awareness" -}
  CURLOPT_COOKIEFILE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- What version to specifically try to use.
     See CURL_SSLVERSION defines below. -}
  CURLOPT_SSLVERSION : CurlOption CURLOPTTYPE_LONG

  {- What kind of HTTP time condition to use, see defines -}
  CURLOPT_TIMECONDITION : CurlOption CURLOPTTYPE_LONG

  {- Time to use with the above condition. Specified in number of seconds
     since 1 Jan 1970 -}
  CURLOPT_TIMEVALUE : CurlOption CURLOPTTYPE_LONG

  {- 35 = OBSOLETE -}

  {- Custom request, for customizing the get command like
     HTTP: DELETE, TRACE and others
     FTP: to use a different list command
     -}
  CURLOPT_CUSTOMREQUEST : CurlOption CURLOPTTYPE_STRINGPOINT

  {- FILE handle to use instead of stderr -}
  CURLOPT_STDERR : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- 38 is not used -}

  {- send linked-list of post-transfer QUOTE commands -}
  CURLOPT_POSTQUOTE : CurlOption CURLOPTTYPE_SLISTPOINT

  CURLOPT_OBSOLETE40 : CurlOption CURLOPTTYPE_OBJECTPOINT {- OBSOLETE, do not use! -}

  CURLOPT_VERBOSE : CurlOption CURLOPTTYPE_LONG      {- talk a lot -}
  CURLOPT_HEADER : CurlOption CURLOPTTYPE_LONG       {- throw the header out too -}
  CURLOPT_NOPROGRESS : CurlOption CURLOPTTYPE_LONG   {- shut off the progress meter -}
  CURLOPT_NOBODY : CurlOption CURLOPTTYPE_LONG       {- use HEAD to get http document -}
  CURLOPT_FAILONERROR : CurlOption CURLOPTTYPE_LONG  {- no output on http error codes >= 400 -}
  CURLOPT_UPLOAD : CurlOption CURLOPTTYPE_LONG       {- this is an upload -}
  CURLOPT_POST : CurlOption CURLOPTTYPE_LONG         {- HTTP POST method -}
  CURLOPT_DIRLISTONLY : CurlOption CURLOPTTYPE_LONG  {- bare names when listing directories -}

  CURLOPT_APPEND : CurlOption CURLOPTTYPE_LONG       {- Append instead of overwrite on upload! -}

  {- Specify whether to read the user+password from the .netrc or the URL.
   * This must be one of the CURL_NETRC_* enums below. -}
  CURLOPT_NETRC : CurlOption CURLOPTTYPE_LONG

  CURLOPT_FOLLOWLOCATION : CurlOption CURLOPTTYPE_LONG  {- use Location: Luke! -}

  CURLOPT_TRANSFERTEXT : CurlOption CURLOPTTYPE_LONG {- transfer data in text/ASCII format -}
  CURLOPT_PUT : CurlOption CURLOPTTYPE_LONG          {- HTTP PUT -}

  {- 55 = OBSOLETE -}

  {- DEPRECATED
   * Function that will be called instead of the internal progress display
   * function. This function should be defined as the curl_progress_callback
   * prototype defines. -}
  CURLOPT_PROGRESSFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Data passed to the CURLOPT_PROGRESSFUNCTION and CURLOPT_XFERINFOFUNCTION
     callbacks -}
  CURLOPT_PROGRESSDATA : CurlOption CURLOPTTYPE_OBJECTPOINT
  CURLOPT_XFERINFODATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- We want the referrer field set automatically when following locations -}
  CURLOPT_AUTOREFERER : CurlOption CURLOPTTYPE_LONG

  {- Port of the proxy, can be set in the proxy string as well with:
     "[host]:[port]" -}
  CURLOPT_PROXYPORT : CurlOption CURLOPTTYPE_LONG

  {- size of the POST input data, if strlen() is not good to use -}
  CURLOPT_POSTFIELDSIZE : CurlOption CURLOPTTYPE_LONG

  {- tunnel non-http operations through a HTTP proxy -}
  CURLOPT_HTTPPROXYTUNNEL : CurlOption CURLOPTTYPE_LONG

  {- Set the interface string to use as outgoing network interface -}
  CURLOPT_INTERFACE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set the krb4/5 security level, this also enables krb4/5 awareness.  This
   * is a string, 'clear', 'safe', 'confidential' or 'private'.  If the string
   * is set but doesn't match one of these, 'private' will be used.  -}
  CURLOPT_KRBLEVEL : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set if we should verify the peer in ssl handshake, set 1 to verify. -}
  CURLOPT_SSL_VERIFYPEER : CurlOption CURLOPTTYPE_LONG

  {- The CApath or CAfile used to validate the peer certificate
     this option is used only if SSL_VERIFYPEER is true -}
  CURLOPT_CAINFO : CurlOption CURLOPTTYPE_STRINGPOINT

  {- 66 = OBSOLETE -}
  {- 67 = OBSOLETE -}

  {- Maximum number of http redirects to follow -}
  CURLOPT_MAXREDIRS : CurlOption CURLOPTTYPE_LONG

  {- Pass a CURLOPTTYPE_LONG set to 1 to get the date of the requested document (if
     possible)! Pass a zero to shut it off. -}
  CURLOPT_FILETIME : CurlOption CURLOPTTYPE_LONG

  {- This points to a linked list of telnet options -}
  CURLOPT_TELNETOPTIONS : CurlOption CURLOPTTYPE_SLISTPOINT

  {- Max amount of cached alive connections -}
  CURLOPT_MAXCONNECTS : CurlOption CURLOPTTYPE_LONG

  CURLOPT_OBSOLETE72 : CurlOption CURLOPTTYPE_LONG {- OBSOLETE, do not use! -}

  {- 73 = OBSOLETE -}

  {- Set to explicitly use a new connection for the upcoming transfer.
     Do not use this unless you're absolutely sure of this, as it makes the
     operation slower and is less friendly for the network. -}
  CURLOPT_FRESH_CONNECT : CurlOption CURLOPTTYPE_LONG

  {- Set to explicitly forbid the upcoming transfer's connection to be re-used
     when done. Do not use this unless you're absolutely sure of this, as it
     makes the operation slower and is less friendly for the network. -}
  CURLOPT_FORBID_REUSE : CurlOption CURLOPTTYPE_LONG

  {- Set to a file name that contains random data for libcurl to use to
     seed the random engine when doing SSL connects. -}
  CURLOPT_RANDOM_FILE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set to the Entropy Gathering Daemon socket pathname -}
  CURLOPT_EGDSOCKET : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Time-out connect operations after this amount of seconds, if connects are
     OK within this time, then fine... This only aborts the connect phase. -}
  CURLOPT_CONNECTTIMEOUT : CurlOption CURLOPTTYPE_LONG

  {- Function that will be called to store headers (instead of fwrite). The
   * parameters will use fwrite() syntax, make sure to follow them. -}
  CURLOPT_HEADERFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Set this to force the HTTP request to get back to GET. Only really usable
     if POST, PUT or a custom request have been used first.
   -}
  CURLOPT_HTTPGET : CurlOption CURLOPTTYPE_LONG

  {- Set if we should verify the Common name from the peer certificate in ssl
   * handshake, set 1 to check existence, 2 to ensure that it matches the
   * provided hostname. -}
  CURLOPT_SSL_VERIFYHOST : CurlOption CURLOPTTYPE_LONG

  {- Specify which file name to write all known cookies in after completed
     operation. Set file name to "-" (dash) to make it go to stdout. -}
  CURLOPT_COOKIEJAR : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Specify which SSL ciphers to use -}
  CURLOPT_SSL_CIPHER_LIST : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Specify which HTTP version to use! This must be set to one of the
     CURL_HTTP_VERSION* enums set below. -}
  CURLOPT_HTTP_VERSION : CurlOption CURLOPTTYPE_LONG

  {- Specifically switch on or off the FTP engine's use of the EPSV command. By
     default, that one will always be attempted before the more traditional
     PASV command. -}
  CURLOPT_FTP_USE_EPSV : CurlOption CURLOPTTYPE_LONG

  {- type of the file keeping your SSL-certificate ("DER", "PEM", "ENG") -}
  CURLOPT_SSLCERTTYPE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- name of the file keeping your private SSL-key -}
  CURLOPT_SSLKEY : CurlOption CURLOPTTYPE_STRINGPOINT

  {- type of the file keeping your private SSL-key ("DER", "PEM", "ENG") -}
  CURLOPT_SSLKEYTYPE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- crypto engine for the SSL-sub system -}
  CURLOPT_SSLENGINE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- set the crypto engine for the SSL-sub system as default
     the param has no meaning...
   -}
  CURLOPT_SSLENGINE_DEFAULT : CurlOption CURLOPTTYPE_LONG

  {- Non-zero value means to use the global dns cache -}
  CURLOPT_DNS_USE_GLOBAL_CACHE : CurlOption CURLOPTTYPE_LONG {- DEPRECATED, do not use! -}

  {- DNS cache timeout -}
  CURLOPT_DNS_CACHE_TIMEOUT : CurlOption CURLOPTTYPE_LONG

  {- send linked-list of pre-transfer QUOTE commands -}
  CURLOPT_PREQUOTE : CurlOption CURLOPTTYPE_SLISTPOINT

  {- set the debug function -}
  CURLOPT_DEBUGFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- set the data for the debug function -}
  CURLOPT_DEBUGDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- mark this as start of a cookie session -}
  CURLOPT_COOKIESESSION : CurlOption CURLOPTTYPE_LONG

  {- The CApath directory used to validate the peer certificate
     this option is used only if SSL_VERIFYPEER is true -}
  CURLOPT_CAPATH : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Instruct libcurl to use a smaller receive buffer -}
  CURLOPT_BUFFERSIZE : CurlOption CURLOPTTYPE_LONG

  {- Instruct libcurl to not use any signal/alarm handlers, even when using
     timeouts. This option is useful for multi-threaded applications.
     See libcurl-the-guide for more background information. -}
  CURLOPT_NOSIGNAL : CurlOption CURLOPTTYPE_LONG

  {- Provide a CURLShare for mutexing non-ts data -}
  CURLOPT_SHARE : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- indicates type of proxy. accepted values are CURLPROXY_HTTP (default),
     CURLPROXY_HTTPS, CURLPROXY_SOCKS4, CURLPROXY_SOCKS4A and
     CURLPROXY_SOCKS5. -}
  CURLOPT_PROXYTYPE : CurlOption CURLOPTTYPE_LONG

  {- Set the Accept-Encoding string. Use this to tell a server you would like
     the response to be compressed. Before 7.21.6, this was known as
     CURLOPT_ENCODING -}
  CURLOPT_ACCEPT_ENCODING : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set pointer to private data -}
  CURLOPT_PRIVATE : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- Set aliases for HTTP 200 in the HTTP Response header -}
  CURLOPT_HTTP200ALIASES : CurlOption CURLOPTTYPE_SLISTPOINT

  {- Continue to send authentication (user+password) when following locations,
     even when hostname changed. This can potentially send off the name
     and password to whatever host the server decides. -}
  CURLOPT_UNRESTRICTED_AUTH : CurlOption CURLOPTTYPE_LONG

  {- Specifically switch on or off the FTP engine's use of the EPRT command (
     it also disables the LPRT attempt). By default, those ones will always be
     attempted before the good old traditional PORT command. -}
  CURLOPT_FTP_USE_EPRT : CurlOption CURLOPTTYPE_LONG

  {- Set this to a bitmask value to enable the particular authentications
     methods you like. Use this in combination with CURLOPT_USERPWD.
     Note that setting multiple bits may cause extra network round-trips. -}
  CURLOPT_HTTPAUTH : CurlOption CURLOPTTYPE_LONG

  {- Set the ssl context callback function, currently only for OpenSSL or
     WolfSSL ssl_ctx, or mbedTLS mbedtls_ssl_config in the second argument.
     The function must match the curl_ssl_ctx_callback prototype. -}
  CURLOPT_SSL_CTX_FUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Set the userdata for the ssl context callback function's third
     argument -}
  CURLOPT_SSL_CTX_DATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- FTP Option that causes missing dirs to be created on the remote server.
     In 7.19.4 we introduced the convenience enums for this option using the
     CURLFTP_CREATE_DIR prefix.
  -}
  CURLOPT_FTP_CREATE_MISSING_DIRS : CurlOption CURLOPTTYPE_LONG

  {- Set this to a bitmask value to enable the particular authentications
     methods you like. Use this in combination with CURLOPT_PROXYUSERPWD.
     Note that setting multiple bits may cause extra network round-trips. -}
  CURLOPT_PROXYAUTH : CurlOption CURLOPTTYPE_LONG

  {- FTP option that changes the timeout, in seconds, associated with
     getting a response.  This is different from transfer timeout time and
     essentially places a demand on the FTP server to acknowledge commands
     in a timely manner. -}
  CURLOPT_FTP_RESPONSE_TIMEOUT : CurlOption CURLOPTTYPE_LONG
  CURLOPT_SERVER_RESPONSE_TIMEOUT : CurlOption CURLOPTTYPE_LONG

  {- Set this option to one of the CURL_IPRESOLVE_* defines (see below) to
     tell libcurl to resolve names to those IP versions only. This only has
     affect on systems with support for more than one, i.e IPv4 _and_ IPv6. -}
  CURLOPT_IPRESOLVE : CurlOption CURLOPTTYPE_LONG

  {- Set this option to limit the size of a file that will be downloaded from
     an HTTP or FTP server.

     Note there is also _LARGE version which adds large file support for
     platforms which have larger CURLOPTTYPE_OFF_T sizes.  See MAXFILESIZE_LARGE below. -}
  CURLOPT_MAXFILESIZE : CurlOption CURLOPTTYPE_LONG

  {- See the comment for INFILESIZE above, but in short, specifies
   * the size of the file being uploaded.  -1 means unknown.
   -}
  CURLOPT_INFILESIZE_LARGE : CurlOption CURLOPTTYPE_OFF_T

  {- Sets the continuation offset.  There is also a CURLOPTTYPE_LONG version of this;
   * look above for RESUME_FROM.
   -}
  CURLOPT_RESUME_FROM_LARGE : CurlOption CURLOPTTYPE_OFF_T

  {- Sets the maximum size of data that will be downloaded from
   * an HTTP or FTP server.  See MAXFILESIZE above for the CURLOPTTYPE_LONG version.
   -}
  CURLOPT_MAXFILESIZE_LARGE : CurlOption CURLOPTTYPE_OFF_T

  {- Set this option to the file name of your .netrc file you want libcurl
     to parse (using the CURLOPT_NETRC option). If not set, libcurl will do
     a poor attempt to find the user's home directory and check for a .netrc
     file in there. -}
  CURLOPT_NETRC_FILE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Enable SSL/TLS for FTP, pick one of:
     CURLUSESSL_TRY     - try using SSL, proceed anyway otherwise
     CURLUSESSL_CONTROL - SSL for the control connection or fail
     CURLUSESSL_ALL     - SSL for all communication or fail
  -}
  CURLOPT_USE_SSL : CurlOption CURLOPTTYPE_LONG

  {- The _LARGE version of the standard POSTFIELDSIZE option -}
  CURLOPT_POSTFIELDSIZE_LARGE : CurlOption CURLOPTTYPE_OFF_T

  {- Enable/disable the TCP Nagle algorithm -}
  CURLOPT_TCP_NODELAY : CurlOption CURLOPTTYPE_LONG

  {- 122 OBSOLETE, used in 7.12.3. Gone in 7.13.0 -}
  {- 123 OBSOLETE. Gone in 7.16.0 -}
  {- 124 OBSOLETE, used in 7.12.3. Gone in 7.13.0 -}
  {- 125 OBSOLETE, used in 7.12.3. Gone in 7.13.0 -}
  {- 126 OBSOLETE, used in 7.12.3. Gone in 7.13.0 -}
  {- 127 OBSOLETE. Gone in 7.16.0 -}
  {- 128 OBSOLETE. Gone in 7.16.0 -}

  {- When FTP over SSL/TLS is selected (with CURLOPT_USE_SSL), this option
     can be used to change libcurl's default action which is to first try
     "AUTH SSL" and then "AUTH TLS" in this order, and proceed when a OK
     response has been received.

     Available parameters are:
     CURLFTPAUTH_DEFAULT - let libcurl decide
     CURLFTPAUTH_SSL     - try "AUTH SSL" first, then TLS
     CURLFTPAUTH_TLS     - try "AUTH TLS" first, then SSL
  -}
  CURLOPT_FTPSSLAUTH : CurlOption CURLOPTTYPE_LONG

  CURLOPT_IOCTLFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT
  CURLOPT_IOCTLDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- 132 OBSOLETE. Gone in 7.16.0 -}
  {- 133 OBSOLETE. Gone in 7.16.0 -}

  {- zero terminated string for pass on to the FTP server when asked for
     "account" info -}
  CURLOPT_FTP_ACCOUNT : CurlOption CURLOPTTYPE_STRINGPOINT

  {- feed cookie into cookie engine -}
  CURLOPT_COOKIELIST : CurlOption CURLOPTTYPE_STRINGPOINT

  {- ignore Content-Length -}
  CURLOPT_IGNORE_CONTENT_LENGTH : CurlOption CURLOPTTYPE_LONG

  {- Set to non-zero to skip the IP address received in a 227 PASV FTP server
     response. Typically used for FTP-SSL purposes but is not restricted to
     that. libcurl will then instead use the same IP address it used for the
     control connection. -}
  CURLOPT_FTP_SKIP_PASV_IP : CurlOption CURLOPTTYPE_LONG

  {- Select "file method" to use when doing FTP, see the curl_ftpmethod
     above. -}
  CURLOPT_FTP_FILEMETHOD : CurlOption CURLOPTTYPE_LONG

  {- Local port number to bind the socket to -}
  CURLOPT_LOCALPORT : CurlOption CURLOPTTYPE_LONG

  {- Number of ports to try, including the first one set with LOCALPORT.
     Thus, setting it to 1 will make no additional attempts but the first.
  -}
  CURLOPT_LOCALPORTRANGE : CurlOption CURLOPTTYPE_LONG

  {- no transfer, set up connection and let application use the socket by
     extracting it with CURLINFO_LASTSOCKET -}
  CURLOPT_CONNECT_ONLY : CurlOption CURLOPTTYPE_LONG

  {- Function that will be called to convert from the
     network encoding (instead of using the iconv calls in libcurl) -}
  CURLOPT_CONV_FROM_NETWORK_FUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Function that will be called to convert to the
     network encoding (instead of using the iconv calls in libcurl) -}
  CURLOPT_CONV_TO_NETWORK_FUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Function that will be called to convert from UTF8
     (instead of using the iconv calls in libcurl)
     Note that this is used only for SSL certificate processing -}
  CURLOPT_CONV_FROM_UTF8_FUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- if the connection proceeds too quickly then need to slow it down -}
  {- limit-rate: maximum number of bytes per second to send or receive -}
  CURLOPT_MAX_SEND_SPEED_LARGE : CurlOption CURLOPTTYPE_OFF_T
  CURLOPT_MAX_RECV_SPEED_LARGE : CurlOption CURLOPTTYPE_OFF_T

  {- Pointer to command string to send if USER/PASS fails. -}
  CURLOPT_FTP_ALTERNATIVE_TO_USER : CurlOption CURLOPTTYPE_STRINGPOINT

  {- callback function for setting socket options -}
  CURLOPT_SOCKOPTFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT
  CURLOPT_SOCKOPTDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- set to 0 to disable session ID re-use for this transfer, default is
     enabled (== 1) -}
  CURLOPT_SSL_SESSIONID_CACHE : CurlOption CURLOPTTYPE_LONG

  {- allowed SSH authentication methods -}
  CURLOPT_SSH_AUTH_TYPES : CurlOption CURLOPTTYPE_LONG

  {- Used by scp/sftp to do public/private key authentication -}
  CURLOPT_SSH_PUBLIC_KEYFILE : CurlOption CURLOPTTYPE_STRINGPOINT
  CURLOPT_SSH_PRIVATE_KEYFILE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Send CCC (Clear Command Channel) after authentication -}
  CURLOPT_FTP_SSL_CCC : CurlOption CURLOPTTYPE_LONG

  {- Same as TIMEOUT and CONNECTTIMEOUT, but with ms resolution -}
  CURLOPT_TIMEOUT_MS : CurlOption CURLOPTTYPE_LONG
  CURLOPT_CONNECTTIMEOUT_MS : CurlOption CURLOPTTYPE_LONG

  {- set to zero to disable the libcurl's decoding and thus pass the raw body
     data to the application even when it is encoded/compressed -}
  CURLOPT_HTTP_TRANSFER_DECODING : CurlOption CURLOPTTYPE_LONG
  CURLOPT_HTTP_CONTENT_DECODING : CurlOption CURLOPTTYPE_LONG

  {- Permission used when creating new files and directories on the remote
     server for protocols that support it, SFTP/SCP/FILE -}
  CURLOPT_NEW_FILE_PERMS : CurlOption CURLOPTTYPE_LONG
  CURLOPT_NEW_DIRECTORY_PERMS : CurlOption CURLOPTTYPE_LONG

  {- Set the behaviour of POST when redirecting. Values must be set to one
     of CURL_REDIR* defines below. This used to be called CURLOPT_POST301 -}
  CURLOPT_POSTREDIR : CurlOption CURLOPTTYPE_LONG

  {- used by scp/sftp to verify the host's public key -}
  CURLOPT_SSH_HOST_PUBLIC_KEY_MD5 : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Callback function for opening socket (instead of socket(2)). Optionally,
     callback is able change the address or refuse to connect returning
     CURL_SOCKET_BAD.  The callback should have type
     curl_opensocket_callback -}
  CURLOPT_OPENSOCKETFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT
  CURLOPT_OPENSOCKETDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- POST volatile input fields. -}
  CURLOPT_COPYPOSTFIELDS : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- set transfer mode (;type=<a|i>) when doing FTP via an HTTP proxy -}
  CURLOPT_PROXY_TRANSFER_MODE : CurlOption CURLOPTTYPE_LONG

  {- Callback function for seeking in the input stream -}
  CURLOPT_SEEKFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT
  CURLOPT_SEEKDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- CRL file -}
  CURLOPT_CRLFILE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Issuer certificate -}
  CURLOPT_ISSUERCERT : CurlOption CURLOPTTYPE_STRINGPOINT

  {- (IPv6) Address scope -}
  CURLOPT_ADDRESS_SCOPE : CurlOption CURLOPTTYPE_LONG

  {- Collect certificate chain info and allow it to get retrievable with
     CURLINFO_CERTINFO after the transfer is complete. -}
  CURLOPT_CERTINFO : CurlOption CURLOPTTYPE_LONG

  {- "name" and "pwd" to use when fetching. -}
  CURLOPT_USERNAME : CurlOption CURLOPTTYPE_STRINGPOINT
  CURLOPT_PASSWORD : CurlOption CURLOPTTYPE_STRINGPOINT

    {- "name" and "pwd" to use with Proxy when fetching. -}
  CURLOPT_PROXYUSERNAME : CurlOption CURLOPTTYPE_STRINGPOINT
  CURLOPT_PROXYPASSWORD : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Comma separated list of hostnames defining no-proxy zones. These should
     match both hostnames directly, and hostnames within a domain. For
     example, local.com will match local.com and www.local.com, but NOT
     notlocal.com or www.notlocal.com. For compatibility with other
     implementations of this, .local.com will be considered to be the same as
     local.com. A single * is the only valid wildcard, and effectively
     disables the use of proxy. -}
  CURLOPT_NOPROXY : CurlOption CURLOPTTYPE_STRINGPOINT

  {- block size for TFTP transfers -}
  CURLOPT_TFTP_BLKSIZE : CurlOption CURLOPTTYPE_LONG

  {- Socks Service -}
  CURLOPT_SOCKS5_GSSAPI_SERVICE : CurlOption CURLOPTTYPE_STRINGPOINT {- DEPRECATED, do not use! -}

  {- Socks Service -}
  CURLOPT_SOCKS5_GSSAPI_NEC : CurlOption CURLOPTTYPE_LONG

  {- set the bitmask for the protocols that are allowed to be used for the
     transfer, which thus helps the app which takes URLs from users or other
     external inputs and want to restrict what protocol(s) to deal
     with. Defaults to CURLPROTO_ALL. -}
  CURLOPT_PROTOCOLS : CurlOption CURLOPTTYPE_LONG

  {- set the bitmask for the protocols that libcurl is allowed to follow to,
     as a subset of the CURLOPT_PROTOCOLS ones. That means the protocol needs
     to be set in both bitmasks to be allowed to get redirected to. -}
  CURLOPT_REDIR_PROTOCOLS : CurlOption CURLOPTTYPE_LONG

  {- set the SSH knownhost file name to use -}
  CURLOPT_SSH_KNOWNHOSTS : CurlOption CURLOPTTYPE_STRINGPOINT

  {- set the SSH host key callback, must point to a curl_sshkeycallback
     function -}
  CURLOPT_SSH_KEYFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- set the SSH host key callback custom pointer -}
  CURLOPT_SSH_KEYDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- set the SMTP mail originator -}
  CURLOPT_MAIL_FROM : CurlOption CURLOPTTYPE_STRINGPOINT

  {- set the list of SMTP mail receiver(s) -}
  CURLOPT_MAIL_RCPT : CurlOption CURLOPTTYPE_SLISTPOINT

  {- FTP: send PRET before PASV -}
  CURLOPT_FTP_USE_PRET : CurlOption CURLOPTTYPE_LONG

  {- RTSP request method (OPTIONS, SETUP, PLAY, etc...) -}
  CURLOPT_RTSP_REQUEST : CurlOption CURLOPTTYPE_LONG

  {- The RTSP session identifier -}
  CURLOPT_RTSP_SESSION_ID : CurlOption CURLOPTTYPE_STRINGPOINT

  {- The RTSP stream URI -}
  CURLOPT_RTSP_STREAM_URI : CurlOption CURLOPTTYPE_STRINGPOINT

  {- The Transport: header to use in RTSP requests -}
  CURLOPT_RTSP_TRANSPORT : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Manually initialize the client RTSP CSeq for this handle -}
  CURLOPT_RTSP_CLIENT_CSEQ : CurlOption CURLOPTTYPE_LONG

  {- Manually initialize the server RTSP CSeq for this handle -}
  CURLOPT_RTSP_SERVER_CSEQ : CurlOption CURLOPTTYPE_LONG

  {- The stream to pass to INTERLEAVEFUNCTION. -}
  CURLOPT_INTERLEAVEDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- Let the application define a custom write method for RTP data -}
  CURLOPT_INTERLEAVEFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Turn on wildcard matching -}
  CURLOPT_WILDCARDMATCH : CurlOption CURLOPTTYPE_LONG

  {- Directory matching callback called before downloading of an
     individual file (chunk) started -}
  CURLOPT_CHUNK_BGN_FUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Directory matching callback called after the file (chunk)
     was downloaded, or skipped -}
  CURLOPT_CHUNK_END_FUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Change match (fnmatch-like) callback for wildcard matching -}
  CURLOPT_FNMATCH_FUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- Let the application define custom chunk data pointer -}
  CURLOPT_CHUNK_DATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- FNMATCH_FUNCTION user pointer -}
  CURLOPT_FNMATCH_DATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- send linked-list of name:port:address sets -}
  CURLOPT_RESOLVE : CurlOption CURLOPTTYPE_SLISTPOINT

  {- Set a username for authenticated TLS -}
  CURLOPT_TLSAUTH_USERNAME : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set a password for authenticated TLS -}
  CURLOPT_TLSAUTH_PASSWORD : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set authentication type for authenticated TLS -}
  CURLOPT_TLSAUTH_TYPE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set to 1 to enable the "TE:" header in HTTP requests to ask for
     compressed transfer-encoded responses. Set to 0 to disable the use of TE:
     in outgoing requests. The current default is 0, but it might change in a
     future libcurl release.

     libcurl will ask for the compressed methods it knows of, and if that
     isn't any, it will not ask for transfer-encoding at all even if this
     option is set to 1.

  -}
  CURLOPT_TRANSFER_ENCODING : CurlOption CURLOPTTYPE_LONG

  {- Callback function for closing socket (instead of close(2)). The callback
     should have type curl_closesocket_callback -}
  CURLOPT_CLOSESOCKETFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT
  CURLOPT_CLOSESOCKETDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- allow GSSAPI credential delegation -}
  CURLOPT_GSSAPI_DELEGATION : CurlOption CURLOPTTYPE_LONG

  {- Set the name servers to use for DNS resolution -}
  CURLOPT_DNS_SERVERS : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Time-out accept operations (currently for FTP only) after this amount
     of milliseconds. -}
  CURLOPT_ACCEPTTIMEOUT_MS : CurlOption CURLOPTTYPE_LONG

  {- Set TCP keepalive -}
  CURLOPT_TCP_KEEPALIVE : CurlOption CURLOPTTYPE_LONG

  {- non-universal keepalive knobs (Linux, AIX, HP-UX, more) -}
  CURLOPT_TCP_KEEPIDLE : CurlOption CURLOPTTYPE_LONG
  CURLOPT_TCP_KEEPINTVL : CurlOption CURLOPTTYPE_LONG

  {- Enable/disable specific SSL features with a bitmask, see CURLSSLOPT_* -}
  CURLOPT_SSL_OPTIONS : CurlOption CURLOPTTYPE_LONG

  {- Set the SMTP auth originator -}
  CURLOPT_MAIL_AUTH : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Enable/disable SASL initial response -}
  CURLOPT_SASL_IR : CurlOption CURLOPTTYPE_LONG

  {- Function that will be called instead of the internal progress display
   * function. This function should be defined as the curl_xferinfo_callback
   * prototype defines. (Deprecates CURLOPT_PROGRESSFUNCTION) -}
  CURLOPT_XFERINFOFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- The XOAUTH2 bearer token -}
  CURLOPT_XOAUTH2_BEARER : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set the interface string to use as outgoing network
   * interface for DNS requests.
   * Only supported by the c-ares DNS backend -}
  CURLOPT_DNS_INTERFACE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set the local IPv4 address to use for outgoing DNS requests.
   * Only supported by the c-ares DNS backend -}
  CURLOPT_DNS_LOCAL_IP4 : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set the local IPv6 address to use for outgoing DNS requests.
   * Only supported by the c-ares DNS backend -}
  CURLOPT_DNS_LOCAL_IP6 : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set authentication options directly -}
  CURLOPT_LOGIN_OPTIONS : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Enable/disable TLS NPN extension (http2 over ssl might fail without) -}
  CURLOPT_SSL_ENABLE_NPN : CurlOption CURLOPTTYPE_LONG

  {- Enable/disable TLS ALPN extension (http2 over ssl might fail without) -}
  CURLOPT_SSL_ENABLE_ALPN : CurlOption CURLOPTTYPE_LONG

  {- Time to wait for a response to a HTTP request containing an
   * Expect: 100-continue header before sending the data anyway. -}
  CURLOPT_EXPECT_100_TIMEOUT_MS : CurlOption CURLOPTTYPE_LONG

  {- This points to a linked list of headers used for proxy requests only,
     struct curl_slist kind -}
  CURLOPT_PROXYHEADER : CurlOption CURLOPTTYPE_SLISTPOINT

  {- Pass in a bitmask of "header options" -}
  CURLOPT_HEADEROPT : CurlOption CURLOPTTYPE_LONG

  {- The public key in DER form used to validate the peer public key
     this option is used only if SSL_VERIFYPEER is true -}
  CURLOPT_PINNEDPUBLICKEY : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Path to Unix domain socket -}
  CURLOPT_UNIX_SOCKET_PATH : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set if we should verify the certificate status. -}
  CURLOPT_SSL_VERIFYSTATUS : CurlOption CURLOPTTYPE_LONG

  {- Set if we should enable TLS false start. -}
  CURLOPT_SSL_FALSESTART : CurlOption CURLOPTTYPE_LONG

  {- Do not squash dot-dot sequences -}
  CURLOPT_PATH_AS_IS : CurlOption CURLOPTTYPE_LONG

  {- Proxy Service Name -}
  CURLOPT_PROXY_SERVICE_NAME : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Service Name -}
  CURLOPT_SERVICE_NAME : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Wait/don't wait for pipe/mutex to clarify -}
  CURLOPT_PIPEWAIT : CurlOption CURLOPTTYPE_LONG

  {- Set the protocol used when curl is given a URL without a protocol -}
  CURLOPT_DEFAULT_PROTOCOL : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set stream weight, 1 - 256 (default is 16) -}
  CURLOPT_STREAM_WEIGHT : CurlOption CURLOPTTYPE_LONG

  {- Set stream dependency on another CURL handle -}
  CURLOPT_STREAM_DEPENDS : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- Set E-xclusive stream dependency on another CURL handle -}
  CURLOPT_STREAM_DEPENDS_E : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- Do not send any tftp option requests to the server -}
  CURLOPT_TFTP_NO_OPTIONS : CurlOption CURLOPTTYPE_LONG

  {- Linked-list of host:port:connect-to-host:connect-to-port,
     overrides the URL's host:port (only for the network layer) -}
  CURLOPT_CONNECT_TO : CurlOption CURLOPTTYPE_SLISTPOINT

  {- Set TCP Fast Open -}
  CURLOPT_TCP_FASTOPEN : CurlOption CURLOPTTYPE_LONG

  {- Continue to send data if the server responds early with an
   * HTTP status code >= 300 -}
  CURLOPT_KEEP_SENDING_ON_ERROR : CurlOption CURLOPTTYPE_LONG

  {- The CApath or CAfile used to validate the proxy certificate
     this option is used only if PROXY_SSL_VERIFYPEER is true -}
  CURLOPT_PROXY_CAINFO : CurlOption CURLOPTTYPE_STRINGPOINT

  {- The CApath directory used to validate the proxy certificate
     this option is used only if PROXY_SSL_VERIFYPEER is true -}
  CURLOPT_PROXY_CAPATH : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set if we should verify the proxy in ssl handshake,
     set 1 to verify. -}
  CURLOPT_PROXY_SSL_VERIFYPEER : CurlOption CURLOPTTYPE_LONG

  {- Set if we should verify the Common name from the proxy certificate in ssl
   * handshake, set 1 to check existence, 2 to ensure that it matches
   * the provided hostname. -}
  CURLOPT_PROXY_SSL_VERIFYHOST : CurlOption CURLOPTTYPE_LONG

  {- What version to specifically try to use for proxy.
     See CURL_SSLVERSION defines below. -}
  CURLOPT_PROXY_SSLVERSION : CurlOption CURLOPTTYPE_LONG

  {- Set a username for authenticated TLS for proxy -}
  CURLOPT_PROXY_TLSAUTH_USERNAME : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set a password for authenticated TLS for proxy -}
  CURLOPT_PROXY_TLSAUTH_PASSWORD : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Set authentication type for authenticated TLS for proxy -}
  CURLOPT_PROXY_TLSAUTH_TYPE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- name of the file keeping your private SSL-certificate for proxy -}
  CURLOPT_PROXY_SSLCERT : CurlOption CURLOPTTYPE_STRINGPOINT

  {- type of the file keeping your SSL-certificate ("DER", "PEM", "ENG") for
     proxy -}
  CURLOPT_PROXY_SSLCERTTYPE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- name of the file keeping your private SSL-key for proxy -}
  CURLOPT_PROXY_SSLKEY : CurlOption CURLOPTTYPE_STRINGPOINT

  {- type of the file keeping your private SSL-key ("DER", "PEM", "ENG") for
     proxy -}
  CURLOPT_PROXY_SSLKEYTYPE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- password for the SSL private key for proxy -}
  CURLOPT_PROXY_KEYPASSWD : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Specify which SSL ciphers to use for proxy -}
  CURLOPT_PROXY_SSL_CIPHER_LIST : CurlOption CURLOPTTYPE_STRINGPOINT

  {- CRL file for proxy -}
  CURLOPT_PROXY_CRLFILE : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Enable/disable specific SSL features with a bitmask for proxy, see
     CURLSSLOPT_* -}
  CURLOPT_PROXY_SSL_OPTIONS : CurlOption CURLOPTTYPE_LONG

  {- Name of pre proxy to use. -}
  CURLOPT_PRE_PROXY : CurlOption CURLOPTTYPE_STRINGPOINT

  {- The public key in DER form used to validate the proxy public key
     this option is used only if PROXY_SSL_VERIFYPEER is true -}
  CURLOPT_PROXY_PINNEDPUBLICKEY : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Path to an abstract Unix domain socket -}
  CURLOPT_ABSTRACT_UNIX_SOCKET : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Suppress proxy CONNECT response headers from user callbacks -}
  CURLOPT_SUPPRESS_CONNECT_HEADERS : CurlOption CURLOPTTYPE_LONG

  {- The request target, instead of extracted from the URL -}
  CURLOPT_REQUEST_TARGET : CurlOption CURLOPTTYPE_STRINGPOINT

  {- bitmask of allowed auth methods for connections to SOCKS5 proxies -}
  CURLOPT_SOCKS5_AUTH : CurlOption CURLOPTTYPE_LONG

  {- Enable/disable SSH compression -}
  CURLOPT_SSH_COMPRESSION : CurlOption CURLOPTTYPE_LONG

  {- Post MIME data. -}
  CURLOPT_MIMEPOST : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- Time to use with the CURLOPT_TIMECONDITION. Specified in number of
     seconds since 1 Jan 1970. -}
  CURLOPT_TIMEVALUE_LARGE : CurlOption CURLOPTTYPE_OFF_T

  {- Head start in milliseconds to give happy eyeballs. -}
  CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS : CurlOption CURLOPTTYPE_LONG

  {- Function that will be called before a resolver request is made -}
  CURLOPT_RESOLVER_START_FUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- User data to pass to the resolver start callback. -}
  CURLOPT_RESOLVER_START_DATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- send HAProxy PROXY protocol header? -}
  CURLOPT_HAPROXYPROTOCOL : CurlOption CURLOPTTYPE_LONG

  {- shuffle addresses before use when DNS returns multiple -}
  CURLOPT_DNS_SHUFFLE_ADDRESSES : CurlOption CURLOPTTYPE_LONG

  {- Specify which TLS 1.3 ciphers suites to use -}
  CURLOPT_TLS13_CIPHERS : CurlOption CURLOPTTYPE_STRINGPOINT
  CURLOPT_PROXY_TLS13_CIPHERS : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Disallow specifying username/login in URL. -}
  CURLOPT_DISALLOW_USERNAME_IN_URL : CurlOption CURLOPTTYPE_LONG

  {- DNS-over-HTTPS URL -}
  CURLOPT_DOH_URL : CurlOption CURLOPTTYPE_STRINGPOINT

  {- Preferred buffer size to use for uploads -}
  CURLOPT_UPLOAD_BUFFERSIZE : CurlOption CURLOPTTYPE_LONG

  {- Time in ms between connection upkeep calls for CURLOPTTYPE_LONG-lived connections. -}
  CURLOPT_UPKEEP_INTERVAL_MS : CurlOption CURLOPTTYPE_LONG

  {- Specify URL using CURL URL API. -}
  CURLOPT_CURLU : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- add trailing data just after no more data is available -}
  CURLOPT_TRAILERFUNCTION : CurlOption CURLOPTTYPE_FUNCTIONPOINT

  {- pointer to be passed to HTTP_TRAILER_FUNCTION -}
  CURLOPT_TRAILERDATA : CurlOption CURLOPTTYPE_OBJECTPOINT

  {- set this to 1L to allow HTTP/0.9 responses or 0L to disallow -}
  CURLOPT_HTTP09_ALLOWED : CurlOption CURLOPTTYPE_LONG

  {- alt-svc control bitmask -}
  CURLOPT_ALTSVC_CTRL : CurlOption CURLOPTTYPE_LONG

  {- alt-svc cache file name to possibly read from/write to -}
  CURLOPT_ALTSVC : CurlOption CURLOPTTYPE_STRINGPOINT

  {- maximum age of a connection to consider it for reuse (in seconds) -}
  CURLOPT_MAXAGE_CONN : CurlOption CURLOPTTYPE_LONG

  {- SASL authorisation identity -}
  CURLOPT_SASL_AUTHZID : CurlOption CURLOPTTYPE_STRINGPOINT

  -- CURLOPT_LASTENTRY : CurlOption CURLOPTTYPE_LONG {- the last unused -}

export
{ty: _} -> ToCode (CurlOption ty) where
  toCode CURLOPT_WRITEDATA = 1 + toCode ty
  toCode CURLOPT_URL = 2 + toCode ty
  toCode CURLOPT_PORT = 3 + toCode ty
  toCode CURLOPT_PROXY = 4 + toCode ty
  toCode CURLOPT_USERPWD = 5 + toCode ty
  toCode CURLOPT_PROXYUSERPWD = 6 + toCode ty
  toCode CURLOPT_RANGE = 7 + toCode ty
  toCode CURLOPT_READDATA = 9 + toCode ty
  toCode CURLOPT_ERRORBUFFER = 10 + toCode ty
  toCode CURLOPT_WRITEFUNCTION = 11 + toCode ty
  toCode CURLOPT_READFUNCTION = 12 + toCode ty
  toCode CURLOPT_TIMEOUT = 13 + toCode ty
  toCode CURLOPT_INFILESIZE = 14 + toCode ty
  toCode CURLOPT_POSTFIELDS = 15 + toCode ty
  toCode CURLOPT_REFERER = 16 + toCode ty
  toCode CURLOPT_FTPPORT = 17 + toCode ty
  toCode CURLOPT_USERAGENT = 18 + toCode ty
  toCode CURLOPT_LOW_SPEED_LIMIT = 19 + toCode ty
  toCode CURLOPT_LOW_SPEED_TIME = 20 + toCode ty
  toCode CURLOPT_RESUME_FROM = 21 + toCode ty
  toCode CURLOPT_COOKIE = 22 + toCode ty
  toCode CURLOPT_HTTPHEADER = 23 + toCode ty
  toCode CURLOPT_HTTPPOST = 24 + toCode ty
  toCode CURLOPT_SSLCERT = 25 + toCode ty
  toCode CURLOPT_KEYPASSWD = 26 + toCode ty
  toCode CURLOPT_CRLF = 27 + toCode ty
  toCode CURLOPT_QUOTE = 28 + toCode ty
  toCode CURLOPT_HEADERDATA = 29 + toCode ty
  toCode CURLOPT_COOKIEFILE = 31 + toCode ty
  toCode CURLOPT_SSLVERSION = 32 + toCode ty
  toCode CURLOPT_TIMECONDITION = 33 + toCode ty
  toCode CURLOPT_TIMEVALUE = 34 + toCode ty
  toCode CURLOPT_CUSTOMREQUEST = 36 + toCode ty
  toCode CURLOPT_STDERR = 37 + toCode ty
  toCode CURLOPT_POSTQUOTE = 39 + toCode ty
  toCode CURLOPT_OBSOLETE40 = 40 + toCode ty
  toCode CURLOPT_VERBOSE = 41 + toCode ty
  toCode CURLOPT_HEADER = 42 + toCode ty
  toCode CURLOPT_NOPROGRESS = 43 + toCode ty
  toCode CURLOPT_NOBODY = 44 + toCode ty
  toCode CURLOPT_FAILONERROR = 45 + toCode ty
  toCode CURLOPT_UPLOAD = 46 + toCode ty
  toCode CURLOPT_POST = 47 + toCode ty
  toCode CURLOPT_DIRLISTONLY = 48 + toCode ty
  toCode CURLOPT_APPEND = 50 + toCode ty
  toCode CURLOPT_NETRC = 51 + toCode ty
  toCode CURLOPT_FOLLOWLOCATION = 52 + toCode ty
  toCode CURLOPT_TRANSFERTEXT = 53 + toCode ty
  toCode CURLOPT_PUT = 54 + toCode ty
  toCode CURLOPT_PROGRESSFUNCTION = 56 + toCode ty
  toCode CURLOPT_PROGRESSDATA = 57 + toCode ty
  toCode CURLOPT_XFERINFODATA = 57 + toCode ty
  toCode CURLOPT_AUTOREFERER = 58 + toCode ty
  toCode CURLOPT_PROXYPORT = 59 + toCode ty
  toCode CURLOPT_POSTFIELDSIZE = 60 + toCode ty
  toCode CURLOPT_HTTPPROXYTUNNEL = 61 + toCode ty
  toCode CURLOPT_INTERFACE = 62 + toCode ty
  toCode CURLOPT_KRBLEVEL = 63 + toCode ty
  toCode CURLOPT_SSL_VERIFYPEER = 64 + toCode ty
  toCode CURLOPT_CAINFO = 65 + toCode ty
  toCode CURLOPT_MAXREDIRS = 68 + toCode ty
  toCode CURLOPT_FILETIME = 69 + toCode ty
  toCode CURLOPT_TELNETOPTIONS = 70 + toCode ty
  toCode CURLOPT_MAXCONNECTS = 71 + toCode ty
  toCode CURLOPT_OBSOLETE72 = 72 + toCode ty
  toCode CURLOPT_FRESH_CONNECT = 74 + toCode ty
  toCode CURLOPT_FORBID_REUSE = 75 + toCode ty
  toCode CURLOPT_RANDOM_FILE = 76 + toCode ty
  toCode CURLOPT_EGDSOCKET = 77 + toCode ty
  toCode CURLOPT_CONNECTTIMEOUT = 78 + toCode ty
  toCode CURLOPT_HEADERFUNCTION = 79 + toCode ty
  toCode CURLOPT_HTTPGET = 80 + toCode ty
  toCode CURLOPT_SSL_VERIFYHOST = 81 + toCode ty
  toCode CURLOPT_COOKIEJAR = 82 + toCode ty
  toCode CURLOPT_SSL_CIPHER_LIST = 83 + toCode ty
  toCode CURLOPT_HTTP_VERSION = 84 + toCode ty
  toCode CURLOPT_FTP_USE_EPSV = 85 + toCode ty
  toCode CURLOPT_SSLCERTTYPE = 86 + toCode ty
  toCode CURLOPT_SSLKEY = 87 + toCode ty
  toCode CURLOPT_SSLKEYTYPE = 88 + toCode ty
  toCode CURLOPT_SSLENGINE = 89 + toCode ty
  toCode CURLOPT_SSLENGINE_DEFAULT = 90 + toCode ty
  toCode CURLOPT_DNS_USE_GLOBAL_CACHE = 91 + toCode ty
  toCode CURLOPT_DNS_CACHE_TIMEOUT = 92 + toCode ty
  toCode CURLOPT_PREQUOTE = 93 + toCode ty
  toCode CURLOPT_DEBUGFUNCTION = 94 + toCode ty
  toCode CURLOPT_DEBUGDATA = 95 + toCode ty
  toCode CURLOPT_COOKIESESSION = 96 + toCode ty
  toCode CURLOPT_CAPATH = 97 + toCode ty
  toCode CURLOPT_BUFFERSIZE = 98 + toCode ty
  toCode CURLOPT_NOSIGNAL = 99 + toCode ty
  toCode CURLOPT_SHARE = 100 + toCode ty
  toCode CURLOPT_PROXYTYPE = 101 + toCode ty
  toCode CURLOPT_ACCEPT_ENCODING = 102 + toCode ty
  toCode CURLOPT_PRIVATE = 103 + toCode ty
  toCode CURLOPT_HTTP200ALIASES = 104 + toCode ty
  toCode CURLOPT_UNRESTRICTED_AUTH = 105 + toCode ty
  toCode CURLOPT_FTP_USE_EPRT = 106 + toCode ty
  toCode CURLOPT_HTTPAUTH = 107 + toCode ty
  toCode CURLOPT_SSL_CTX_FUNCTION = 108 + toCode ty
  toCode CURLOPT_SSL_CTX_DATA = 109 + toCode ty
  toCode CURLOPT_FTP_CREATE_MISSING_DIRS = 110 + toCode ty
  toCode CURLOPT_PROXYAUTH = 111 + toCode ty
  toCode CURLOPT_FTP_RESPONSE_TIMEOUT = 112 + toCode ty
  toCode CURLOPT_SERVER_RESPONSE_TIMEOUT = 112 + toCode ty
  toCode CURLOPT_IPRESOLVE = 113 + toCode ty
  toCode CURLOPT_MAXFILESIZE = 114 + toCode ty
  toCode CURLOPT_INFILESIZE_LARGE = 115 + toCode ty
  toCode CURLOPT_RESUME_FROM_LARGE = 116 + toCode ty
  toCode CURLOPT_MAXFILESIZE_LARGE = 117 + toCode ty
  toCode CURLOPT_NETRC_FILE = 118 + toCode ty
  toCode CURLOPT_USE_SSL = 119 + toCode ty
  toCode CURLOPT_POSTFIELDSIZE_LARGE = 120 + toCode ty
  toCode CURLOPT_TCP_NODELAY = 121 + toCode ty
  toCode CURLOPT_FTPSSLAUTH = 129 + toCode ty
  toCode CURLOPT_IOCTLFUNCTION = 130 + toCode ty
  toCode CURLOPT_IOCTLDATA = 131 + toCode ty
  toCode CURLOPT_FTP_ACCOUNT = 134 + toCode ty
  toCode CURLOPT_COOKIELIST = 135 + toCode ty
  toCode CURLOPT_IGNORE_CONTENT_LENGTH = 136 + toCode ty
  toCode CURLOPT_FTP_SKIP_PASV_IP = 137 + toCode ty
  toCode CURLOPT_FTP_FILEMETHOD = 138 + toCode ty
  toCode CURLOPT_LOCALPORT = 139 + toCode ty
  toCode CURLOPT_LOCALPORTRANGE = 140 + toCode ty
  toCode CURLOPT_CONNECT_ONLY = 141 + toCode ty
  toCode CURLOPT_CONV_FROM_NETWORK_FUNCTION = 142 + toCode ty
  toCode CURLOPT_CONV_TO_NETWORK_FUNCTION = 143 + toCode ty
  toCode CURLOPT_CONV_FROM_UTF8_FUNCTION = 144 + toCode ty
  toCode CURLOPT_MAX_SEND_SPEED_LARGE = 145 + toCode ty
  toCode CURLOPT_MAX_RECV_SPEED_LARGE = 146 + toCode ty
  toCode CURLOPT_FTP_ALTERNATIVE_TO_USER = 147 + toCode ty
  toCode CURLOPT_SOCKOPTFUNCTION = 148 + toCode ty
  toCode CURLOPT_SOCKOPTDATA = 149 + toCode ty
  toCode CURLOPT_SSL_SESSIONID_CACHE = 150 + toCode ty
  toCode CURLOPT_SSH_AUTH_TYPES = 151 + toCode ty
  toCode CURLOPT_SSH_PUBLIC_KEYFILE = 152 + toCode ty
  toCode CURLOPT_SSH_PRIVATE_KEYFILE = 153 + toCode ty
  toCode CURLOPT_FTP_SSL_CCC = 154 + toCode ty
  toCode CURLOPT_TIMEOUT_MS = 155 + toCode ty
  toCode CURLOPT_CONNECTTIMEOUT_MS = 156 + toCode ty
  toCode CURLOPT_HTTP_TRANSFER_DECODING = 157 + toCode ty
  toCode CURLOPT_HTTP_CONTENT_DECODING = 158 + toCode ty
  toCode CURLOPT_NEW_FILE_PERMS = 159 + toCode ty
  toCode CURLOPT_NEW_DIRECTORY_PERMS = 160 + toCode ty
  toCode CURLOPT_POSTREDIR = 161 + toCode ty
  toCode CURLOPT_SSH_HOST_PUBLIC_KEY_MD5 = 162 + toCode ty
  toCode CURLOPT_OPENSOCKETFUNCTION = 163 + toCode ty
  toCode CURLOPT_OPENSOCKETDATA = 164 + toCode ty
  toCode CURLOPT_COPYPOSTFIELDS = 165 + toCode ty
  toCode CURLOPT_PROXY_TRANSFER_MODE = 166 + toCode ty
  toCode CURLOPT_SEEKFUNCTION = 167 + toCode ty
  toCode CURLOPT_SEEKDATA = 168 + toCode ty
  toCode CURLOPT_CRLFILE = 169 + toCode ty
  toCode CURLOPT_ISSUERCERT = 170 + toCode ty
  toCode CURLOPT_ADDRESS_SCOPE = 171 + toCode ty
  toCode CURLOPT_CERTINFO = 172 + toCode ty
  toCode CURLOPT_USERNAME = 173 + toCode ty
  toCode CURLOPT_PASSWORD = 174 + toCode ty
  toCode CURLOPT_PROXYUSERNAME = 175 + toCode ty
  toCode CURLOPT_PROXYPASSWORD = 176 + toCode ty
  toCode CURLOPT_NOPROXY = 177 + toCode ty
  toCode CURLOPT_TFTP_BLKSIZE = 178 + toCode ty
  toCode CURLOPT_SOCKS5_GSSAPI_SERVICE = 179 + toCode ty
  toCode CURLOPT_SOCKS5_GSSAPI_NEC = 180 + toCode ty
  toCode CURLOPT_PROTOCOLS = 181 + toCode ty
  toCode CURLOPT_REDIR_PROTOCOLS = 182 + toCode ty
  toCode CURLOPT_SSH_KNOWNHOSTS = 183 + toCode ty
  toCode CURLOPT_SSH_KEYFUNCTION = 184 + toCode ty
  toCode CURLOPT_SSH_KEYDATA = 185 + toCode ty
  toCode CURLOPT_MAIL_FROM = 186 + toCode ty
  toCode CURLOPT_MAIL_RCPT = 187 + toCode ty
  toCode CURLOPT_FTP_USE_PRET = 188 + toCode ty
  toCode CURLOPT_RTSP_REQUEST = 189 + toCode ty
  toCode CURLOPT_RTSP_SESSION_ID = 190 + toCode ty
  toCode CURLOPT_RTSP_STREAM_URI = 191 + toCode ty
  toCode CURLOPT_RTSP_TRANSPORT = 192 + toCode ty
  toCode CURLOPT_RTSP_CLIENT_CSEQ = 193 + toCode ty
  toCode CURLOPT_RTSP_SERVER_CSEQ = 194 + toCode ty
  toCode CURLOPT_INTERLEAVEDATA = 195 + toCode ty
  toCode CURLOPT_INTERLEAVEFUNCTION = 196 + toCode ty
  toCode CURLOPT_WILDCARDMATCH = 197 + toCode ty
  toCode CURLOPT_CHUNK_BGN_FUNCTION = 198 + toCode ty
  toCode CURLOPT_CHUNK_END_FUNCTION = 199 + toCode ty
  toCode CURLOPT_FNMATCH_FUNCTION = 200 + toCode ty
  toCode CURLOPT_CHUNK_DATA = 201 + toCode ty
  toCode CURLOPT_FNMATCH_DATA = 202 + toCode ty
  toCode CURLOPT_RESOLVE = 203 + toCode ty
  toCode CURLOPT_TLSAUTH_USERNAME = 204 + toCode ty
  toCode CURLOPT_TLSAUTH_PASSWORD = 205 + toCode ty
  toCode CURLOPT_TLSAUTH_TYPE = 206 + toCode ty
  toCode CURLOPT_TRANSFER_ENCODING = 207 + toCode ty
  toCode CURLOPT_CLOSESOCKETFUNCTION = 208 + toCode ty
  toCode CURLOPT_CLOSESOCKETDATA = 209 + toCode ty
  toCode CURLOPT_GSSAPI_DELEGATION = 210 + toCode ty
  toCode CURLOPT_DNS_SERVERS = 211 + toCode ty
  toCode CURLOPT_ACCEPTTIMEOUT_MS = 212 + toCode ty
  toCode CURLOPT_TCP_KEEPALIVE = 213 + toCode ty
  toCode CURLOPT_TCP_KEEPIDLE = 214 + toCode ty
  toCode CURLOPT_TCP_KEEPINTVL = 215 + toCode ty
  toCode CURLOPT_SSL_OPTIONS = 216 + toCode ty
  toCode CURLOPT_MAIL_AUTH = 217 + toCode ty
  toCode CURLOPT_SASL_IR = 218 + toCode ty
  toCode CURLOPT_XFERINFOFUNCTION = 219 + toCode ty
  toCode CURLOPT_XOAUTH2_BEARER = 220 + toCode ty
  toCode CURLOPT_DNS_INTERFACE = 221 + toCode ty
  toCode CURLOPT_DNS_LOCAL_IP4 = 222 + toCode ty
  toCode CURLOPT_DNS_LOCAL_IP6 = 223 + toCode ty
  toCode CURLOPT_LOGIN_OPTIONS = 224 + toCode ty
  toCode CURLOPT_SSL_ENABLE_NPN = 225 + toCode ty
  toCode CURLOPT_SSL_ENABLE_ALPN = 226 + toCode ty
  toCode CURLOPT_EXPECT_100_TIMEOUT_MS = 227 + toCode ty
  toCode CURLOPT_PROXYHEADER = 228 + toCode ty
  toCode CURLOPT_HEADEROPT = 229 + toCode ty
  toCode CURLOPT_PINNEDPUBLICKEY = 230 + toCode ty
  toCode CURLOPT_UNIX_SOCKET_PATH = 231 + toCode ty
  toCode CURLOPT_SSL_VERIFYSTATUS = 232 + toCode ty
  toCode CURLOPT_SSL_FALSESTART = 233 + toCode ty
  toCode CURLOPT_PATH_AS_IS = 234 + toCode ty
  toCode CURLOPT_PROXY_SERVICE_NAME = 235 + toCode ty
  toCode CURLOPT_SERVICE_NAME = 236 + toCode ty
  toCode CURLOPT_PIPEWAIT = 237 + toCode ty
  toCode CURLOPT_DEFAULT_PROTOCOL = 238 + toCode ty
  toCode CURLOPT_STREAM_WEIGHT = 239 + toCode ty
  toCode CURLOPT_STREAM_DEPENDS = 240 + toCode ty
  toCode CURLOPT_STREAM_DEPENDS_E = 241 + toCode ty
  toCode CURLOPT_TFTP_NO_OPTIONS = 242 + toCode ty
  toCode CURLOPT_CONNECT_TO = 243 + toCode ty
  toCode CURLOPT_TCP_FASTOPEN = 244 + toCode ty
  toCode CURLOPT_KEEP_SENDING_ON_ERROR = 245 + toCode ty
  toCode CURLOPT_PROXY_CAINFO = 246 + toCode ty
  toCode CURLOPT_PROXY_CAPATH = 247 + toCode ty
  toCode CURLOPT_PROXY_SSL_VERIFYPEER = 248 + toCode ty
  toCode CURLOPT_PROXY_SSL_VERIFYHOST = 249 + toCode ty
  toCode CURLOPT_PROXY_SSLVERSION = 250 + toCode ty
  toCode CURLOPT_PROXY_TLSAUTH_USERNAME = 251 + toCode ty
  toCode CURLOPT_PROXY_TLSAUTH_PASSWORD = 252 + toCode ty
  toCode CURLOPT_PROXY_TLSAUTH_TYPE = 253 + toCode ty
  toCode CURLOPT_PROXY_SSLCERT = 254 + toCode ty
  toCode CURLOPT_PROXY_SSLCERTTYPE = 255 + toCode ty
  toCode CURLOPT_PROXY_SSLKEY = 256 + toCode ty
  toCode CURLOPT_PROXY_SSLKEYTYPE = 257 + toCode ty
  toCode CURLOPT_PROXY_KEYPASSWD = 258 + toCode ty
  toCode CURLOPT_PROXY_SSL_CIPHER_LIST = 259 + toCode ty
  toCode CURLOPT_PROXY_CRLFILE = 260 + toCode ty
  toCode CURLOPT_PROXY_SSL_OPTIONS = 261 + toCode ty
  toCode CURLOPT_PRE_PROXY = 262 + toCode ty
  toCode CURLOPT_PROXY_PINNEDPUBLICKEY = 263 + toCode ty
  toCode CURLOPT_ABSTRACT_UNIX_SOCKET = 264 + toCode ty
  toCode CURLOPT_SUPPRESS_CONNECT_HEADERS = 265 + toCode ty
  toCode CURLOPT_REQUEST_TARGET = 266 + toCode ty
  toCode CURLOPT_SOCKS5_AUTH = 267 + toCode ty
  toCode CURLOPT_SSH_COMPRESSION = 268 + toCode ty
  toCode CURLOPT_MIMEPOST = 269 + toCode ty
  toCode CURLOPT_TIMEVALUE_LARGE = 270 + toCode ty
  toCode CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS = 271 + toCode ty
  toCode CURLOPT_RESOLVER_START_FUNCTION = 272 + toCode ty
  toCode CURLOPT_RESOLVER_START_DATA = 273 + toCode ty
  toCode CURLOPT_HAPROXYPROTOCOL = 274 + toCode ty
  toCode CURLOPT_DNS_SHUFFLE_ADDRESSES = 275 + toCode ty
  toCode CURLOPT_TLS13_CIPHERS = 276 + toCode ty
  toCode CURLOPT_PROXY_TLS13_CIPHERS = 277 + toCode ty
  toCode CURLOPT_DISALLOW_USERNAME_IN_URL = 278 + toCode ty
  toCode CURLOPT_DOH_URL = 279 + toCode ty
  toCode CURLOPT_UPLOAD_BUFFERSIZE = 280 + toCode ty
  toCode CURLOPT_UPKEEP_INTERVAL_MS = 281 + toCode ty
  toCode CURLOPT_CURLU = 282 + toCode ty
  toCode CURLOPT_TRAILERFUNCTION = 283 + toCode ty
  toCode CURLOPT_TRAILERDATA = 284 + toCode ty
  toCode CURLOPT_HTTP09_ALLOWED = 285 + toCode ty
  toCode CURLOPT_ALTSVC_CTRL = 286 + toCode ty
  toCode CURLOPT_ALTSVC = 287 + toCode ty
  toCode CURLOPT_MAXAGE_CONN = 288 + toCode ty
  toCode CURLOPT_SASL_AUTHZID = 289 + toCode ty
  -- toCode CURLOPT_LASTENTRY = 290

public export
Show (CurlOption ty) where
  show = showEnum


public export
paramTy : OptType -> Type
paramTy CURLOPTTYPE_LONG = Int
paramTy CURLOPTTYPE_FUNCTIONPOINT = String -> Int -> Int -> AnyPtr -> IO Int
paramTy CURLOPTTYPE_OBJECTPOINT = AnyPtr
paramTy CURLOPTTYPE_STRINGPOINT = String
paramTy CURLOPTTYPE_SLISTPOINT = AnyPtr
paramTy CURLOPTTYPE_OFF_T = Int
