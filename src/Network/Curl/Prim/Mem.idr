module Network.Curl.Prim.Mem

import public Data.Buffer

-- TODO tests these instances
-- We should be using Nat some places but it's not especially convenient

%foreign "scheme:blodwen-new-buffer"
prim_newBuffer' : Int -> PrimIO Buffer

export
newBuffer' : HasIO io => (bytes : Int) -> io Buffer
newBuffer' size = primIO $ prim_newBuffer' size
-- No Maybe, if we can't allocate memory then die.

data PtrType = Raw | GC

||| If we extend this to support pointer math keep in mind that Buffer isn't
||| exactly a ptr since it has an intrinsic size, it just works as a ptr for
||| foreign calls.
public export
data ForeignPtr : Type -> Type where
  MkFP : Buffer -> ForeignPtr a
%name MkFP fp

||| Minimal definitions size,pokeByteOff,peekByteOff
public export
interface Storable (a : Type) where
  ||| Required
  ||| size in bits of `a` considering alignment
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


||| This won't continue existing once sizeOf in Storable is fixed/changed
export
size : Storable a => Int
size = unsafePerformIO $ do pure $ sizeOf {a} (MkFP !(newBuffer' 0))

||| better to rely on withAllocElems, to handle freeing if needed down the road
export
allocBytes : HasIO io => Int -> io (ForeignPtr a)
allocBytes n = MkFP <$> newBuffer' n

||| better to rely on withAllocElems, to handle freeing if needed down the road
export
allocElems : HasIO io => Storable a => Int -> io (ForeignPtr a)
allocElems n = MkFP <$> newBuffer' (n * size {a})

||| Copy `len` worth of bytes from one ptr to another
||| offsets are of bytes
export
copyByteOff : HasIO io => ForeignPtr a -> Int -> Int
                       -> ForeignPtr a -> Int -> io ()
copyByteOff (MkFP src) s_off len (MkFP dst) d_off
  = copyData src s_off len dst d_off

||| Copy `len` worth of elements from one ptr to another
||| offsets are of elements
export
copyElemOff : HasIO io => Storable a => ForeignPtr a -> Int -> Int
                       -> ForeignPtr a -> Int -> io ()
copyElemOff (MkFP src) s_off len (MkFP dst) d_off
  = let s = size {a} in copyData src (s_off * s) (len * s) dst (d_off * s)

export
castForeignPtr : ForeignPtr a -> ForeignPtr b
castForeignPtr (MkFP x) = MkFP x

export
unsafeForeignPtrToBuffer : ForeignPtr a -> Buffer
unsafeForeignPtrToBuffer (MkFP b) = b

export
getStringFrom : HasIO io => ForeignPtr Bits8 -> Int -> Int -> io String
getStringFrom (MkFP fp) off len = getString fp off len

-- There a method for getting int and method for getting int32, int32 isn't a
-- distinct type though so I'm not sure what is being implied there. the chez
-- backend has int as 63 bits
export
Storable Int where
  sizeOf _ = 64 -- backend says 63 but alignments would be 64
  peekByteOff (MkFP b) off   = getInt b off
  pokeByteOff (MkFP b) off v = setInt b off v

export
Storable AnyPtr where
  sizeOf _ = 64 -- on my pc currently, implementation will change in the future
  peekByteOff (MkFP b) off   = believe_me <$> getInt b off
  pokeByteOff (MkFP b) off v = setInt b off (believe_me v)

export
Storable (Ptr a) where
  sizeOf _ = 64 -- on my pc currently, implementation will change in the future
  peekByteOff (MkFP b) off   = believe_me <$> getInt b off
  pokeByteOff (MkFP b) off v = setInt b off (believe_me v)

export
Storable Bits8 where
  sizeOf _ = 8
  peekByteOff (MkFP b) off = getBits8 b off
  pokeByteOff (MkFP b) off v = setBits8 b off v

export
Storable Bits16 where
  sizeOf _ = 16
  peekByteOff (MkFP b) off = getBits16 b off
  pokeByteOff (MkFP b) off v = setBits16 b off v

export
Storable Bits32 where
  sizeOf _ = 32
  peekByteOff (MkFP b) off = getBits32 b off
  pokeByteOff (MkFP b) off v = setBits32 b off v

export
Storable Bits64 where
  sizeOf _ = 64
  peekByteOff (MkFP b) off = getBits64 b off
  pokeByteOff (MkFP b) off v = setBits64 b off v

-- This is NOT c's char which is a Bits8.
-- int32 makes a showing here since codepoints come in under 32bits
export
Storable Char where
  sizeOf _ = 32
  peekByteOff (MkFP b) off = cast <$> getInt32 b off
  pokeByteOff (MkFP b) off v = setInt32 b off (cast v)

export
Storable Double where
  sizeOf _ = 64
  peekByteOff (MkFP b) off = getDouble b off
  pokeByteOff (MkFP b) off v = setDouble b off v


-- does nothing, managed by scheme gc atm
free : HasIO io => Buffer -> io ()
free _ = pure ()

export
withAllocBytes : (HasIO io, Storable a) => Int -> (ForeignPtr a -> io b) -> io b
withAllocBytes size act = do buf <- allocElems size
                             res <- act buf
                             -- free buf -- automatic, GC
                             pure res

export
withAllocElems : (HasIO io, Storable a) => Int -> (ForeignPtr a -> io b) -> io b
withAllocElems size act = do buf <- allocElems size
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
