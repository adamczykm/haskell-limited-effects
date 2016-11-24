{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Free
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Data.Aeson
import Data.ByteString.Lazy as BL (ByteString)
import Data.Vinyl
import Control.Lens hiding (Identity)
import Data.Singletons.TH

-- type Interpreter f g = forall a b c. (Member f a, Member g b) => Eff a c ->  Eff b c
-- type (~<) f g = Interpreter f g
----------------------
class Monad m => MonadHttp m where
    get :: Url -> m ByteString
    post :: ToJSON a => Url -> a -> m ByteString

type Url = String

reify :: (forall m. MonadHttp m => m a) -> Free HttpF a
reify = unFreeHttp

newtype FreeHttp a = FreeHttp { unFreeHttp :: Free HttpF a } deriving (Functor, Applicative, Monad)

data HttpF next
    = Get Url (ByteString -> next)
    | Post Url ByteString next

instance Functor HttpF where
  fmap f (Get url n) = Get url (fmap f n)
  fmap f (Post url a n) = Post url a (f n)

instance MonadHttp FreeHttp where
    get url = FreeHttp (liftF (Get url id))
    post url body = FreeHttp (liftF (Post url (encode body) ""))


class Monad m => MonadLog m where
  logM :: String -> m ()

instance MonadLog IO where
  logM = putStrLn

-- instance MonadReader m =>

-- newtype TeletypeM a = TeletypeM { unTeletypeM :: Eff '[Teletype] a}

-- instance (Member Teletype r) => MonadLog (Eff r) where
--   logM = undefined

type InterpreterFor g eff = forall a. (forall f. eff f => f a) -> g a
newtype Interp g eff = Interp {unInterp :: forall a. (forall f. eff f => f a) -> g a}
-----------------------------------------
-- SERVICES
data Service = LoggingService | HttpService
data Temp a = Temp

type family ServicesFam m (s :: Service) :: * where
  ServicesFam g 'LoggingService = Interp g MonadLog
  ServicesFam g 'HttpService = Interp g MonadHttp

newtype Attr f g = Attr { _unAttr :: ServicesFam f g}
makeLenses ''Attr
genSingletons [ ''Service ]

-- type ServiceFun s m = forall ss. (Monad m, s ⊆ ss ) => ReaderT (Rec (Attr m) ss) m ()

-- temp :: ServiceFun '[LoggingService] m
-- temp = do
--   rss <- view (rlens SLoggingService . rsubset) <$> ask
--   lift $ actualStuff . _unAttr $ rss -- ^. (rlens SLoggingService)
--   where
--     actualStuff :: Interp m MonadLog -> m ()
--     actualStuff logSrv = unInterp logSrv $ logM "I've made It"
type ServiceFun s m = forall ss. (Monad m, s ∈ ss ) => ReaderT (Rec (Attr m) ss) m ()

temp :: ServiceFun 'LoggingService m
temp = do
  rss <- view (rlens SLoggingService) <$> ask
  lift $ actualStuff . _unAttr $ rss -- ^. (rlens SLoggingService)
  where
    actualStuff :: Interp m MonadLog -> m ()
    actualStuff logSrv = unInterp logSrv $ logM "I've made It"

data Services eff = Services { runLog  :: eff `InterpreterFor` MonadLog }
-- data Services = Services { runLog  :: IO `InterpreterFor` MonadLog }

testLogService :: IO `InterpreterFor` MonadLog
testLogService action = action

testLogService3 :: Interp IO MonadLog
testLogService3 = Interp testLogService

testLogService4 :: ServicesFam IO 'LoggingService
testLogService4 = Interp testLogService

testLogService2 :: Eff '[Teletype] `InterpreterFor` MonadLog
testLogService2 action = action

type Application a = forall m. Monad m => ReaderT (Services m) m a
-- type Application m = ReaderT (Services m) m

testfun :: Services m -> m ()
testfun services = runLog services $ logM "http://www.google.com"

app1 :: Application ()
-- app1 :: Application IO ()
app1 = do
  ss <- ask
  lift $ runLog ss $ logM "http://www.google.com"


test3 :: IO ()
test3 = void $ runReaderT temp ((Attr testLogService3 :: Attr IO 'LoggingService) :& RNil)

test2 :: IO ()
test2 = void $ runReaderT app1 (Services testLogService)

test :: IO ()
test = void $ runReaderT app1 (Services testLogService)

---------------------------

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Teletype s where
  PutStrLn :: String -> Teletype ()
  GetLine  :: Teletype String
  ExitSuccess :: Teletype ()

instance Show (Teletype s) where
  show GetLine = "GetLine"
  show (PutStrLn s) = "PutStrLn " ++ s
  show ExitSuccess = "ExitSuccess"

instance (Member Teletype r) => MonadLog (Eff r) where
  logM = putTextLn'

putTextLn' :: Member Teletype r => String -> Eff r ()
putTextLn' = send . PutStrLn

getTextLn' :: (Member Teletype r) => Eff r String
getTextLn' = send GetLine

exitSuccess' :: Member Teletype r => Eff r ()
exitSuccess' = send ExitSuccess


-- runTeletypeIO2 :: Eff '[Teletype, IO] w -> IO w
-- runTeletypeIO2 = runM . handleRelay return interp
--   where
--     interp (PutStrLn msg) eff = send _

runTeletypeIO :: Eff '[Teletype] w -> IO w
runTeletypeIO (Val x) = return x
runTeletypeIO (E u q) = case decomp u of
  Right (PutStrLn msg) -> putStrLn msg >> runTeletypeIO (qApp q ())
  Right GetLine        -> getLine >>= \inp -> runTeletypeIO (qApp q inp)
  Right ExitSuccess    -> getLine >>= \_ -> runTeletypeIO (qApp q ())
  Left _               -> error "This cannot happen"

runTeletypePure :: [String] -> Eff '[Teletype] w -> [String]
runTeletypePure inps prog = reverse (go [] inps prog)
  where
    go :: [String] -> [String] -> Eff '[Teletype] w -> [String]
    go acc _ (Val _) = acc
    go acc [] (E u q) = case decomp u of
      Right (PutStrLn msg) -> go (msg:acc) [] (qApp q ())
      Right GetLine        -> go acc [] (Val ())
      Right ExitSuccess    -> go acc [] (Val ())
      Left _               -> go acc [] (Val ())
    go acc inp@(x:xs) (E u q) = case decomp u of
      Right (PutStrLn msg) -> go (msg:acc) inp (qApp q ())
      Right GetLine        -> go acc xs (qApp q x)
      Right ExitSuccess    -> go acc inp (Val ())
      Left _               -> go acc inp (Val ())


testTeletype :: Eff '[Teletype] ()
testTeletype = forever $ getTextLn' >>= \t -> putTextLn' t
-- testTeletype = replicateM_ 2 $ getTextLn' >>= \t -> putTextLn' t


data Log s where
  LogSimple :: String -> Log ()


log' :: Member Log r => String -> Eff r ()
log' = send . LogSimple

runLogIO :: Eff '[Log] w -> IO w
runLogIO (Val x) = return x
runLogIO (E u q) = case decomp u of
  Right (LogSimple msg) -> putStrLn msg >> runLogIO (qApp q ())
  Left _               -> error "This cannot happen"

-- logTeletype :: Log ~< (Teletype w)
-- logTeletype :: Log ~< Teletype
-- logTeletype (Val x) = return x
-- logTeletype (E u q) = case decomp u of
--   Right (LogSimple msg) -> putTextLn' msg >> logTeletype (qApp q ())
--   Left _               -> error "This cannot happen"
