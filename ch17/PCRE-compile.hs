
foreign import ccall unsafe "pcre.h pcre_compile"
c_pcre_compile :: CString
                  -> PCREOption
                  -> Ptr CString
                  -> Ptr CInt
                  -> Ptr Word8
                  -> IO (Ptr PCRE)

type PCRE = ()
