module IdePurescript.Atom.SelectView (selectListViewStatic, selectListViewStaticInline, selectListViewDynamic) where

import Prelude
import Control.Monad.Eff (Eff)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn4, EffFn6, mkEffFn1, runEffFn4, runEffFn6)
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

foreign import selectListViewStaticInlineImpl :: forall eff a. EffFn4 (dom :: DOM | eff)
  (a -> String)
  (EffFn1 (dom :: DOM | eff) a Unit)
  (Nullable String)
  (Array a)
  Unit

selectListViewStaticInline :: forall eff a.
  (a -> String) ->
  (a -> Eff (dom :: DOM | eff) Unit) ->
  Maybe String ->
  (Array a) ->
  Eff (dom :: DOM | eff) Unit
selectListViewStaticInline viewForItem confirmed filterKey items =
  runEffFn4 selectListViewStaticInlineImpl viewForItem (mkEffFn1 confirmed) (toNullable filterKey) items

foreign import selectListViewDynamicImpl :: forall eff a. EffFn6 (dom :: DOM | eff)
  (a -> String)
  (EffFn1 (dom :: DOM | eff) a Unit)
  (Nullable String)
  (String -> String)
  (EffFn1 (dom :: DOM | eff) String (Promise (Array a)))
  Int
  Unit

selectListViewDynamic :: forall eff a.
  (a -> String) ->
  (a -> Eff (dom :: DOM | eff) Unit) ->
  Maybe String ->
  (String -> String) ->
  (String -> Aff (dom :: DOM | eff) (Array a)) ->
  Int ->
  Eff (dom :: DOM | eff) Unit
selectListViewDynamic viewForItem confirmed filterKey filterQuery getItems changeDelay =
  runEffFn6 selectListViewDynamicImpl viewForItem (mkEffFn1 confirmed) (toNullable filterKey) filterQuery
    (mkEffFn1 (\s -> Promise.fromAff (getItems s))) changeDelay
