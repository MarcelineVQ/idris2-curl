module Network.Curl.Prim.Mime

-------------------------------------------------
-- Mime
-------------------------------------------------
-- curl_mime_addpart
-- curl_mime_data
-- curl_mime_data_cb
-- curl_mime_encoder
-- curl_mime_filedata
-- curl_mime_filename
-- curl_mime_free
-- curl_mime_headers
-- curl_mime_init
-- curl_mime_name
-- curl_mime_subparts
-- curl_mime_type
-------------------------------------------------

%foreign "fizz:bop,pop"
fizz : Int -> Int


-- Because multiple flavors of Scheme are supported as Idris backends scheme foreign specifiers can be written to target particular flavors.  
-- The following example shows a foreign declaration that allocates memory in a way specific to the choice of backend. There is no general scheme specifier present that matches every flavor, e.g `scheme:foo`, so the example will only match the specific backends listed.
-- 
-- .. code-block:: idris
-- 
--     %foreign "scheme,chez:foreign-alloc"
--              "scheme,racket:malloc"
--              "C:malloc,libc"
--     allocMem : (bytes : Int) -> PrimIO AnyPtr
-- 
-- .. note:: If your backend is not specified here but defines a C FFI it will be able to make use of the `C:malloc,libc` specifier.