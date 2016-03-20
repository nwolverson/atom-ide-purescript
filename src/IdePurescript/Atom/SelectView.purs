module IdePurescript.Atom.SelectView (selectListViewStatic, selectListViewDynamic) where

import Prelude
import Control.Monad.Eff (Eff)

import Control.Monad.Aff (Aff)
import Data.Function.Eff (EffFn1, EffFn4, EffFn5, mkEffFn1, runEffFn4, runEffFn5)
import DOM (DOM)
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe)
import Control.Promise (Promise)
import Control.Promise as Promise

foreign import selectListViewStaticImpl :: forall eff a. EffFn4 (dom :: DOM | eff)
  (a -> String)
  (EffFn1 (dom :: DOM | eff) a Unit)
  (Nullable String)
  (Array a)
  Unit

selectListViewStatic :: forall eff a.
  (a -> String) ->
  (a -> Eff (dom :: DOM | eff) Unit) ->
  Maybe String ->
  (Array a) ->
  Eff (dom :: DOM | eff) Unit
selectListViewStatic viewForItem confirmed filterKey items =
  runEffFn4 selectListViewStaticImpl viewForItem (mkEffFn1 confirmed) (toNullable filterKey) items

foreign import selectListViewDynamicImpl :: forall eff a. EffFn5 (dom :: DOM | eff)
  (a -> String)
  (EffFn1 (dom :: DOM | eff) a Unit)
  (Nullable String)
  (String -> String)
  (EffFn1 (dom :: DOM | eff) String (Promise (Array a)))
  Unit

selectListViewDynamic :: forall eff a.
  (a -> String) ->
  (a -> Eff (dom :: DOM | eff) Unit) ->
  Maybe String ->
  (String -> String) ->
  (String -> Aff (dom :: DOM | eff) (Array a)) ->
  Eff (dom :: DOM | eff) Unit
selectListViewDynamic viewForItem confirmed filterKey filterQuery getItems =
  runEffFn5 selectListViewDynamicImpl viewForItem (mkEffFn1 confirmed) (toNullable filterKey) filterQuery
    (mkEffFn1 (\s -> Promise.fromAff (getItems s)))
