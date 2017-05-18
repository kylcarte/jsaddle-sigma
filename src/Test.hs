{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.MVar
import Data.Monoid  ( (<>) )
import qualified Data.Foldable as F

import Language.Javascript.JSaddle
  hiding
    ( (<#) , (<##)
    , (#)  , (##)
    , (!)  , (!!)
    )
import qualified Language.Javascript.JSaddle as JS
import Language.Javascript.JSaddle.Warp
import Control.Lens ((&))
import qualified Control.Lens as L
import qualified Numeric.Lens as L

{-
runTest :: IO ()
runTest = run 3709 test

test :: JSM ()
test = do
  doc <- global ?> "document"
  doc ?> "body"
    & "innerHTML" <! "<h1>Kia ora (Hi)</h1>"
  doc & "onclick" <!
    ( fun $ \_ _ [e] -> do
      x <- e ?> "clientX" >>= valToNumber
      y <- e ?> "clientY" >>= valToNumber
      p <- doc & "createElement" #! "p"
      p & "appendChild" #!
        ( doc & "createTextNode" #!
          ("Click " ++ show (x,y))
        )
      doc ?> "body" & "appendChild" #! p
      return ()
    )
  exitMVar <- liftIO newEmptyMVar
  exit <- doc & "createElement" #! "span"
  exit & "appendChild" #!
    ( doc & "createTextNode" #! "Click here to exit" )
  doc ?> "body" & "appendChild" #! exit
  exit & "onclick" <!
    ( fun $ \_ _ _ -> liftIO $ putMVar exitMVar () )
  syncPoint
  ctx <- askJSM
  liftIO $ forkIO $ forever
    $ runJSM &. ctx
    $ nextAnimationFrame $ \t -> do
      exit ?> "style" & "color" <! rgb 0 0 (sineWave t)
      return ()
  liftIO $ takeMVar exitMVar
  doc ?> "body" & "innerHTML" <! "<h1>Ka kite ano (See you later)</h1>"
  return ()
-}

{-
jsCall :: (MakeObject this, ToJSString name, MakeArgs args)
       => name -> args
       -> L.IndexPreservingGetter this (JSM JSVal)
jsCall name args = L.to _
-}

   --  => name -> args
   --  -> this
   --  -> JSM JSVal

-- JSaddle syntax {{{

(<!) :: (MakeObject this, ToJSString name, ToJSVal val)
     => name -> val
     -> this
     -> JSM ()
(n <! v) this = this JS.<# n $ v

(<:!) :: (MakeObject this, ToJSVal val) 
      => Int -> val
      -> this
      -> JSM ()
(i <:! v) this = this JS.<## i $ v

(!>) :: (MakeObject this, ToJSString name, ToJSVal val)
     => val -> name
     -> this
     -> JSM ()
(v !> n) this = this JS.<# n $ v

(!:>) :: (MakeObject this, ToJSVal val) 
      => val -> Int
      -> this
      -> JSM ()
(v !:> i) this = this JS.<## i $ v

infixl 7 <!, <:!, !>, !:>

(?>) :: (MakeObject this, ToJSString name)
     => this
     -> name
     -> JSM JSVal
this ?> n = this JS.! n

(?:>) :: MakeObject this
      => this
      -> Int
      -> JSM JSVal
this ?:> i = this JS.!! i

(<?) :: (MakeObject this, ToJSString name)
     => name
     -> this
     -> JSM JSVal
n <? this = this JS.! n

(<:?) :: MakeObject this
      => Int
      -> this
      -> JSM JSVal
i <:? this = this JS.!! i

infixl 9 ?>, ?:>, <?, <:?

(#) :: (MakeObject this, ToJSString name, MakeArgs args)
    => name -> args
    -> this
    -> JSM JSVal
(n # args) this = this JS.# n $ args

(#:) :: (MakeObject this, MakeArgs args)
     => Int -> args
     -> this
     -> JSM JSVal
(i #: args) this = this JS.## i $ args

(#!) :: (MakeObject this, ToJSString name, ToJSVal arg)
    => name -> arg
    -> this
    -> JSM JSVal
(n #! arg) this = this JS.# n $ [arg]

(#:!) :: (MakeObject this, ToJSVal arg)
     => Int -> arg
     -> this
     -> JSM JSVal
(i #:! arg) this = this JS.## i $ [arg]

infixl 8 #, #:, #!, #:!

-- }}}

-- Foldable syntax {{{

(>>-) :: (Foldable f, Monoid m) => f a -> (a -> m) -> m
(>>-) = flip foldMap
infixl 1 >>-

(<:>) :: (Foldable f, Foldable g) => f a -> g b -> [(a,b)]
a <:> b = F.toList a `zip` F.toList b
infixr 6 <:>

(&.) :: (a -> b -> c) -> b -> a -> c
(&.) = flip
infixl 1 &.

-- }}}

-- Color util {{{

sineWave :: Double -> Integer
sineWave x = floor $ 128 * (sin (3 * x) + 1)

rgb :: Integer -> Integer -> Integer -> String
rgb r g b = '#' : foldMap toHex [r,g,b]
  where
  toHex = pad '0' 2 . (L.hex L.#) . clamp 0 255

pad :: Char -> Int -> String -> String
pad c l s = replicate (max 0 $ l - length s) c ++ s

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = min hi . max lo

-- }}}

