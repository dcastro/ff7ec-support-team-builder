module Utils where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Error.Class as M
import Data.Bounded.Generic (class GenericBottom, genericBottom)
import Data.Either (Either(..))
import Data.Enum.Generic (class GenericEnum, genericSucc)
import Data.Foldable as Fold
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Foreign (MultipleErrors, renderForeignError)
import Partial.Unsafe (unsafeCrashWith)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON as J

{- |
Similar to the function `the` from Idris, used to manually ascribe the type of an expression
in a chain of functions.

https://www.idris-lang.org/docs/idris2/current/prelude_docs/docs/Prelude.Basics.html#Prelude.Basics.the

```
myMap
  # Map.toUnfoldable
  # the @(Array _)
  <#> (\(k /\ v) -> ...)
```
-}
the :: forall @a. a -> a
the = identity

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

logOnLeft :: forall m e a. MonadThrow Unit m => MonadEffect m => (e -> String) -> Either e a -> m a
logOnLeft mkMsg either =
  case either of
    Right a -> pure a
    Left err -> do
      Console.error $ mkMsg err
      throwError unit

renderJsonErr :: MultipleErrors -> String
renderJsonErr errs =
  Fold.intercalate ", " (renderForeignError <$> errs)

listEnum :: forall a rep. Generic a rep => GenericEnum rep => GenericBottom rep => Array a
listEnum = unfoldr (\b -> b >>= next) $ Just genericBottom
  where
  next a = Just $ Tuple a $ genericSucc a

unsafeFromJust :: forall @a. Maybe a -> String -> a
unsafeFromJust mb reason =
  case mb of
    Just a -> a
    Nothing -> unsafeCrashWith reason

newtype MapAsArray k v = MapAsArray (Map k v)

instance WriteForeign (Tuple k v) => WriteForeign (MapAsArray k v) where
  writeImpl (MapAsArray x) = J.writeImpl (Map.toUnfoldable x :: Array (Tuple k v))

instance (ReadForeign (Tuple k v), Ord k) => ReadForeign (MapAsArray k v) where
  readImpl json = do
    arr <- J.readImpl json :: _ (Array (Tuple k v))
    pure $ MapAsArray $ Map.fromFoldable arr

derive instance Newtype (MapAsArray k v) _

newtype SetAsArray a = SetAsArray (Set a)

instance WriteForeign a => WriteForeign (SetAsArray a) where
  writeImpl (SetAsArray x) = J.writeImpl (Set.toUnfoldable x :: Array a)

instance (ReadForeign a, Ord a) => ReadForeign (SetAsArray a) where
  readImpl json = do
    arr <- J.readImpl json :: _ (Array a)
    pure $ SetAsArray $ Set.fromFoldable arr

derive instance Newtype (SetAsArray a) _
