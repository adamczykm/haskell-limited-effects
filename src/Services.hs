{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Services where

import           Control.Lens                 hiding (Identity)
import           Control.Monad
import           Control.Monad.Free
import           Control.Monad.Freer
import           Control.Monad.Freer.Internal
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Data.Aeson
import           Data.ByteString.Lazy.Char8   (ByteString)
import qualified Data.ByteString.Lazy.Char8   as BL
import           Data.Singletons.TH
import           Data.Vinyl

import           Control.Category
import           Prelude                      hiding (id, (.))
import qualified Prelude                      as P


------ Intepreter type ----------

newtype Interpret c d = Interpret (forall n a. d n => (forall m. c m => m a) -> n a)
newtype Interp g eff = Interp {unInterp :: forall a. (forall f. eff f => f a) -> g a}

instance Category Interpret where
  id = Interpret P.id
  Interpret f . Interpret g = Interpret $ \h -> f (g h)

------ Effects interfaces ------

type Url = String

class Monad m => MonadHttp m where
  httpGet :: String -> m String

class Monad m => MonadLog m where
  logM :: String -> m ()

class Monad m => MonadRestApi m where
  getUsersIds :: m [Int]

------ Application monad & services

data Service = RestApiService | LoggingService | HttpService

type family ServicesFam m (s :: Service) :: * where
  ServicesFam g 'RestApiService = Interpret MonadRestApi g
  ServicesFam g 'LoggingService = Interpret MonadLog g
  ServicesFam g 'HttpService = Interpret MonadHttp g

newtype Attr f g = Attr { _unAttr :: ServicesFam f g}

(=::) :: (sing f -> ServicesFam g f -> Attr g f)
(=::) _ = Attr

makeLenses ''Attr
genSingletons [ ''Service ]

type Services eff = Rec (Attr eff) '[ 'RestApiService]
-- type Services eff = Rec (Attr eff) '[ 'RestApiService, 'LoggingService, 'HttpService]

-- (forall n a. d n => (forall m. c m => m a) -> n a)
-- type Application a = forall c m. c m => ReaderT (Services m) m a

----- Application environment mock

type MockMonad r w = RWS r w ()


------ Effects implementations ------

--- real http
newtype HttpApp a = HttpApp { runHttpApp :: IO a}
  deriving (Functor, Applicative, Monad)

instance MonadHttp HttpApp where
  httpGet _ = HttpApp (putStrLn "I've made it!!!!!") >> return "[]" -- should do something real

--- http mock
newtype MockHttp m a = MockHttp { runMockHttp :: m a}
  deriving (Functor, Applicative, Monad)

instance MonadReader r m => MonadReader r (MockHttp m) where
  ask = MockHttp ask
  local f (MockHttp m) = MockHttp $ local f m

instance MonadReader String m => MonadHttp (MockHttp m) where
  httpGet _ = ask

---- rest api defined with monadhttp

data RestApi a = GetUsers ([Int] -> a) deriving Functor

instance MonadRestApi (Free RestApi) where
  getUsersIds = liftF $ GetUsers id

runRestApi :: Interpret MonadRestApi MonadHttp
runRestApi = Interpret $ iterA go where
  go (GetUsers f) = do
    response <- httpGet "url"
    f $ read response

---- rest api service implementations

mockedApiService :: Interpret MonadRestApi (MonadReader String)
mockedApiService = Interpret runMockHttp . runRestApi

productionApiService :: Interpret MonadRestApi MonadIO
productionApiService = (Interpret $ \x -> liftIO $ runHttpApp x) . runRestApi

---- application services implementations



runApp app srv = runReaderT (unApp app) srv

newtype App a = App { unApp :: forall c m. (c m, Monad m ) => ReaderT (Services c) m a  }

type Application a = forall c m. (c m, Monad m) => ReaderT (Services c) m a
type ServiceFun1 s r = forall c m ss. (c m, Monad m, s ∈ ss) => ReaderT (Rec (Attr c) ss) m r


restApiFun :: (forall c m. c m => Interpret MonadRestApi c -> m a) -> ServiceFun1 'RestApiService a
restApiFun f = do
  rss <- view (rlens SRestApiService) <$> ask
  lift $ f . _unAttr $ rss -- ^. (rlens SLoggingService)


logFun :: ServiceFun1 'RestApiService ()
logFun = restApiFun actualStuff
  where
    actualStuff :: forall c m. c m => Interpret MonadRestApi c -> m ()
    actualStuff (Interpret f) = f $ void getUsersIds
    -- actualStuff :: Interp m MonadLog -> m ()
    -- actualStuff logSrv = unInterp logSrv $ logM "I've made It"

-- not legit <- only temporarily
instance (MonadReader String) IO where
  ask = getLine
  -- local f (MockHttp m) = MockHttp $ local f m

app1 :: App ()
app1 = App logFun

test3 :: IO ()
test3 = void $ runApp app1 (rcast productionServices)


-- app1 :: Application ()
-- -- app1 :: Application IO ()
-- app1 = do
--   ss <- ask
--   lift $ 

mockServices :: Services (MonadReader String)
mockServices = (SRestApiService =:: mockedApiService) :& RNil

productionServices :: Services MonadIO
productionServices = (SRestApiService =:: productionApiService) :& RNil

dd :: MonadIO m => m ()
dd = undefined

main :: IO ()
main = dd
