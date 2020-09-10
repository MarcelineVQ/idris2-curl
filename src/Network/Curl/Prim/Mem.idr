module Network.Curl.Prim.Mem

import public Data.Buffer

%foreign "scheme:blodwen-new-buffer"
prim_newBuffer' : Int -> PrimIO Buffer

export
newBuffer' : HasIO io => (bytes : Int) -> io Buffer
newBuffer' size = primIO $ prim_newBuffer' size
-- No Maybe, if we can't allocate memory then die.

data PtrType = Raw | GC

export
data ForeignPtr : Type -> Type where
  MkFP : Buffer -> ForeignPtr a

||| Minimal definitions size,pokeByteOff,peekByteOff
public export
interface Storable (a : Type) where
  ||| Required, size in bits of `a`
  ||| This is _not_ the size of ForeignPtr, ForeignPtr is just an easy proxy
  ||| and is only required due to some limitations currently in default deriving
  sizeOf : ForeignPtr a -> Int

  ||| Required
  peekByteOff : HasIO io => ForeignPtr a -> Int -> io a

  peek : HasIO io => ForeignPtr a -> io a
  peek fp = peekByteOff fp 0
  
  peekElemOff : HasIO io => ForeignPtr a -> Int -> io a
  peekElemOff fp off = peekByteOff fp (off * sizeOf fp)
  
  ||| required
  pokeByteOff : HasIO io => ForeignPtr a -> Int -> a -> io ()

  poke : HasIO io => ForeignPtr a -> a -> io ()
  poke fp = pokeByteOff fp 0

  pokeElemOff : HasIO io => ForeignPtr a -> Int -> a -> io ()
  pokeElemOff fp off v = pokeByteOff fp (off * sizeOf fp) v


allocBytes : HasIO io => Int -> io (ForeignPtr a)
allocBytes n = MkFP <$> newBuffer' n

allocElems : HasIO io => Storable a => Int -> io (ForeignPtr a)
allocElems n = MkFP <$> newBuffer' (n * sizeOf {a} (MkFP !(newBuffer' 0)))


export
castForeignPtr : ForeignPtr a -> ForeignPtr b
castForeignPtr = ?dsffsd

export
unsafeForeignPtrToBuffer : ForeignPtr a -> Buffer
unsafeForeignPtrToBuffer (MkFP b) = b

Storable Int where
  sizeOf _ = 32
  peekByteOff (MkFP b) off = getInt32 b off
  pokeByteOff (MkFP b) off v = setInt32 b off v


Storable Bits8 where
  sizeOf _ = 8
  peekByteOff (MkFP b) off = getBits8 b off
  pokeByteOff (MkFP b) off v = setBits8 b off v


-- does nothing, managed by scheme gc atm
free : HasIO io => Buffer -> io ()
free _ = pure ()

export
withAlloc : (HasIO io, Storable a) => Int -> (ForeignPtr a -> io b) -> io b
withAlloc size act = do buf <- allocElems size
                        res <- act buf
                        -- free buf -- automatic, GC
                        pure res

-- main : IO ()
-- main = do
--   withAlloc {a=Int} 30 $ \ptr => do
--     poke ptr 0 12
--     v <- peek ptr 0
--     printLn v
--     poke ptr 0 15
--     v <- peek ptr 0
--     printLn v
--     -- printLn "last"
--   pure ()
