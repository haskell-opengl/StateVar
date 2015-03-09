{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.StateVar
-- Copyright   :  (c) Edward Kmett 2014-2015, Sven Panne 2009-2014
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- State variables are references in the IO monad, like 'IORef's or parts of
-- the OpenGL state. Note that state variables are not neccessarily writable or
-- readable, they may come in read-only or write-only flavours, too. As a very
-- simple example for a state variable, consider an explicitly allocated memory
-- buffer. This buffer could easily be converted into a 'StateVar':
--
-- @
-- makeStateVarFromPtr :: Storable a => Ptr a -> StateVar a
-- makeStateVarFromPtr p = makeStateVar (peek p) (poke p)
-- @
--
-- The example below puts 11 into a state variable (i.e. into the buffer),
-- increments the contents of the state variable by 22, and finally prints the
-- resulting content:
--
-- @
--   do p <- malloc :: IO (Ptr Int)
--      let v = makeStateVarFromPtr p
--      v $= 11
--      v $~ (+ 22)
--      x <- get v
--      print x
-- @
--
-- However, 'Ptr' can be used directly through the same API:
--
-- @
--   do p <- malloc :: IO (Ptr Int)
--      p $= 11
--      p $~ (+ 22)
--      x <- get p
--      print x
-- @
--
-- 'IORef's are state variables, too, so an example with them looks extremely
-- similiar:
--
-- @
--   do v <- newIORef (0 :: Int)
--      v $= 11
--      v $~ (+ 22)
--      x <- get v
--      print x
-- @
--------------------------------------------------------------------------------

module Data.StateVar
  (
  -- * Readable State Variables
    HasGetter(get)
  , GettableStateVar, makeGettableStateVar
  -- * Writable State Variables
  , HasSetter(($=)), ($=!)
  , SettableStateVar(SettableStateVar), makeSettableStateVar
  -- * Updatable State Variables
  , HasUpdate(($~), ($~!))
  , StateVar(StateVar), makeStateVar
  , mapStateVar
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.IORef
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable

--------------------------------------------------------------------
-- * StateVar
--------------------------------------------------------------------

-- | A concrete implementation of a readable and writable state variable,
-- carrying one IO action to read the value and another IO action to write the
-- new value.
--
-- This data type represents a piece of mutable, imperative state
-- with possible side-effects. These tend to encapsulate all sorts
-- tricky behavior in external libraries, and may well throw
-- exceptions.
--
-- Inhabitants __should__ satsify the following properties.
--
-- In the absence of concurrent mutation from other threads or a
-- thrown exception:
--
-- @
-- do x <- 'get' v; v '$=' y; v '$=' x
-- @
--
-- should restore the previous state.
--
-- Ideally, in the absence of thrown exceptions:
--
-- @
-- v '$=' a >> 'get' v
-- @
--
-- should return @a@, regardless of @a@. In practice some 'StateVar's only
-- permit a very limited range of value assignments, and do not report failure.
data StateVar a = StateVar (IO a) (a -> IO ()) deriving Typeable

-- | Construct a 'StateVar' from two IO actions, one for reading and one for
--- writing.
makeStateVar
  :: IO a         -- ^ getter
  -> (a -> IO ()) -- ^ setter
  -> StateVar a
makeStateVar = StateVar

-- | Change the type of a 'StateVar'
mapStateVar :: (b -> a) -> (a -> b) -> StateVar a -> StateVar b
mapStateVar ba ab (StateVar ga sa) = StateVar (fmap ab ga) (sa . ba)
{-# INLINE mapStateVar #-}

-- | A concrete implementation of a write-only state variable, carrying an IO
-- action to write the new value.
newtype SettableStateVar a = SettableStateVar (a -> IO ())
  deriving Typeable

-- | Construct a 'SettableStateVar' from an IO action for writing.
makeSettableStateVar
  :: (a -> IO ()) -- ^ setter
  -> SettableStateVar a
makeSettableStateVar = SettableStateVar
{-# INLINE makeSettableStateVar #-}

-- | A concrete implementation of a read-only state variable is simply an IO
-- action to read the value.
type GettableStateVar = IO

-- | Construct a 'GettableStateVar' from an IO action.
makeGettableStateVar
  :: IO a -- ^ getter
  -> GettableStateVar a
makeGettableStateVar = id
{-# INLINE makeGettableStateVar #-}

--------------------------------------------------------------------
-- * HasSetter
--------------------------------------------------------------------

infixr 2 $=, $=!

-- | This is the class of all writable state variables.
class HasSetter t a | t -> a where
  -- | Write a new value into a state variable.
  ($=) :: MonadIO m => t -> a -> m ()

-- | This is a variant of '$=' which is strict in the value to be set.
($=!) :: (HasSetter t a, MonadIO m) => t -> a -> m ()
p $=! a = (p $=) $! a
{-# INLINE ($=!) #-}

instance HasSetter (SettableStateVar a) a where
  SettableStateVar f $= a = liftIO (f a)
  {-# INLINE ($=) #-}

instance HasSetter (StateVar a) a where
  StateVar _ s $= a = liftIO $ s a
  {-# INLINE ($=) #-}

instance Storable a => HasSetter (Ptr a) a where
  p $= a = liftIO $ poke p a
  {-# INLINE ($=) #-}

instance HasSetter (IORef a) a where
  p $= a = liftIO $ writeIORef p a
  {-# INLINE ($=) #-}

instance HasSetter (TVar a) a where
  p $= a = liftIO $ atomically $ writeTVar p a
  {-# INLINE ($=) #-}

--------------------------------------------------------------------
-- * HasUpdate
--------------------------------------------------------------------

infixr 2 $~, $~!

class HasSetter t a => HasUpdate t a b | t -> a b where
  -- | Transform the contents of a state variable with a given funtion.
  ($~) :: MonadIO m => t -> (a -> b) -> m ()
  default ($~) :: (MonadIO m, a ~ b, HasGetter t a, HasSetter t a) => t -> (a -> b) -> m ()
  r $~ f = liftIO $ do
    a <- get r
    r $= f a

  -- | This is a variant of '$~' which is strict in the transformed value.
  ($~!) :: MonadIO m => t -> (a -> b) -> m ()
  default ($~!) :: (MonadIO m, a ~ b, HasGetter t a, HasSetter t a) => t -> (a -> b) -> m ()
  r $~! f = liftIO $ do
    a <- get r
    r $=! f a

instance HasUpdate (StateVar a) a a

instance Storable a => HasUpdate (Ptr a) a a

instance HasUpdate (IORef a) a a where
  r $~ f  = liftIO $ atomicModifyIORef r $ \a -> (f a,())
#if __GLASGOW_HASKELL__ >= 706
  r $~! f = liftIO $ atomicModifyIORef' r $ \a -> (f a,())
#else
  r $~! f = liftIO $ do
    s <- atomicModifyIORef r $ \a -> let s = f a in (s, s)
    s `seq` return ()
#endif

instance HasUpdate (TVar a) a a where
  r $~ f = liftIO $ atomically $ do
    a <- readTVar r
    writeTVar r (f a)
  r $~! f = liftIO $ atomically $ do
    a <- readTVar r
    writeTVar r $! f a

--------------------------------------------------------------------
-- * HasGetter
--------------------------------------------------------------------

class HasGetter t a | t -> a where
  get :: MonadIO m => t -> m a

instance HasGetter (StateVar a) a where
  get (StateVar g _) = liftIO g
  {-# INLINE get #-}

instance HasGetter (TVar a) a where
  get = liftIO . atomically . readTVar
  {-# INLINE get #-}

instance HasGetter (IO a) a where
  get = liftIO
  {-# INLINE get #-}

instance HasGetter (STM a) a where
  get = liftIO . atomically
  {-# INLINE get #-}

instance Storable a => HasGetter (Ptr a) a where
  get = liftIO . peek
  {-# INLINE get #-}

instance HasGetter (IORef a) a where
  get = liftIO . readIORef
  {-# INLINE get #-}
