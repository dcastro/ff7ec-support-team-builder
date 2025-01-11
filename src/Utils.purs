module Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Error.Class as M
import Data.Bounded.Generic (class GenericBottom, genericBottom)
import Data.Either (Either(..))
import Data.Enum.Generic (class GenericEnum, genericSucc)
import Data.Foldable as Fold
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Foreign (MultipleErrors, renderForeignError)

whenJust :: forall @a @m. Monad m => Maybe a -> (a -> m Unit) -> m Unit
whenJust mb action =
  case mb of
    Just a -> action a
    Nothing -> pure unit

throwOnNothing :: forall m a. MonadThrow Unit m => m (Maybe a) -> m a
throwOnNothing action =
  action >>= M.liftMaybe unit

logOnNothing :: forall m a. MonadThrow Unit m => MonadEffect m => String -> Maybe a -> m a
logOnNothing msg mb = do
  case mb of
    Just a -> pure a
    Nothing -> do
      Console.log msg
      throwError unit

logOnLeft :: forall m e a. MonadThrow Unit m => MonadEffect m => Either e a -> (e -> String) -> m a
logOnLeft either mkMsg = do
  case either of
    Right a -> pure a
    Left err -> do
      Console.log $ mkMsg err
      throwError unit

renderJsonErr :: MultipleErrors -> String
renderJsonErr errs =
  Fold.intercalate ", " (renderForeignError <$> errs)

listEnum :: forall a rep. Generic a rep => GenericEnum rep => GenericBottom rep => Array a
listEnum = unfoldr (\b -> b >>= next) $ Just genericBottom
  where
  next a = Just $ Tuple a $ genericSucc a
