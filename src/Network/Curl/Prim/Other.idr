module Network.Curl.Prim.Other

import Derive.Prim
%language ElabReflection

-------------------------------------------------
-- Other
-------------------------------------------------
-- curl_formadd
-- curl_formfree
-- curl_formget
-- curl_free
-- curl_getdate

-- curl_pushheader_byname
-- curl_pushheader_bynum
-- curl_share_cleanup
-- curl_share_init
-- curl_share_setopt
-- curl_share_strerror
-- curl_slist_append
-- curl_slist_free_all
-- curl_url
-- curl_url_cleanup
-- curl_url_dup
-- curl_url_get
-- curl_url_set
-- curl_version
-- curl_version_info
-------------------------------------------------

%runElab makeHasIO "curl_free" Export
          `[ %foreign "C:curl_free,libcurl,curl/curl.h"
             export
             prim_curl_free : Ptr Bits8 -> PrimIO () ] --`
