definition module System.IO

import Data.Error
import Data.Either

import Data.Function

import Control.Mapable
import Control.Applyable
import Control.Bindable
import Control.Throwing

/// ## The `IO` Monad

instance Functor IO where
    map g (IO ma) = IO run
    where
        run world
            # (a,world) = ma world
            = case a of
                Left e -> (Left e, world)
                Right a -> (Right $ g a, world)

instance Applicative IO where
    pure a = IO (\world -> (Right a, world))

    (<*>) (IO mf) (IO ma) = IO run
    where
        run world
            # (f,world) = mf world
            = case f of
                Left e -> (Left e, world)
                Right f
                    # (a,world) = ma world
                    = case a of
                        Left e -> (Left e, world)
                        Right a -> (Right $ f a, world)

instance Monad IO where
    (>>=) (IO ma) next = IO run
    where
        run world
            # (a,world) = ma world
            = case a of
                Left e -> (Left e, world)
                Right a
                    # (IO mb) = next a
                    = mb world

instance Throwing IO where
    throw e = IO (\world -> (Left e, world))

/// ## Helpers

execIO :: (IO a) !*World -> *World
execIO (IO ma) world
  # (_, world) = ma world
  = world

evalIO :: (IO a) !*World -> *(Usually a, !*World)
evalIO (IO ma) world = ma world

mapIO :: ((Usually a) -> Usually b) (IO a) -> IO b
mapIO g (IO ma) = IO run
where
    run world
        # (a,world) = ma world
        = (g a, world)

withIO :: (!*World -> *(Usually a, !*World)) -> IO a
withIO f = IO f

/*
withEither :: (Usually a) -> IO a
withEither ea = IO (\world -> (ea, world))

withVoid :: (*World -> *World) -> IO ()
withVoid f = IO (\world
    # world = f world
    = (Right (), world))

withValue :: (*World -> (a, *World)) -> IO a
withValue f = IO (\world
    # (a,world) = f world
    = (Right a, world))

withError :: (e -> Error) (*World -> *(Either e a, *World)) -> IO a
// withError f g = mapIO (mapLeft f) $ withIO g
withError f g = IO (\world
    # (ea,world) = g world
    = case ea of
        Left e -> (Left $ f e, world)
        Right a -> (Right a, world))

mapWorld :: ((Either e a) -> Either e` a) (*World -> *(Either e a, *World)) -> IO e` a
mapWorld f g = mapIO f $ withIO g
*/
