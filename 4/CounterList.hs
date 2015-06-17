{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
module CounterList where

import Reflex.Dom
import qualified Counter
import Data.Map (Map)
import qualified Data.Map as Map

-- MODEL

type ID = Int

data Model = Model { counters :: [(ID, Counter.Model)]
                   , nextID :: ID }

initModel :: Model
initModel = Model { counters = []
                  , nextID = 0 }

-- UPDATE

data Action
  = Insert
  | Remove ID
  | Modify ID Counter.Action

update :: Action -> Model -> Model
update Insert model =
  let newCounter = (nextID model, Counter.initCounter 0)
      newCounters = (counters model) ++ [ newCounter ]
  in model { counters = newCounters
           , nextID = (nextID model) + 1 }
update (Remove id) model =
  model { counters = filter (\(counterID,_) -> counterID /= id) (counters model) }
update (Modify id counterAction) model =
  model { counters = map updateCounter (counters model) }
  where
   updateCounter (counterID, counterModel) =
     if counterID == id
     then (counterID, Counter.update counterAction counterModel)
     else (counterID, counterModel)

-- VIEW

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model =
  el "div" $ do
    (insert, _) <- el' "button" $ text "Add"
    counters <- mapDyn (Map.fromList . counters) model
    -- we enhance the normal counter events with the id
    counterEvs <- listWithKey counters (\k m -> do
                                           (modEv, remEv) <- Counter.viewWithRemoveButton m
                                           return (fmap (k,) modEv, fmap (k,) remEv))
    -- because we follow Elm example, let's simplify here and choose first event
    modEvt <- mapDyn (leftmost . map fst . Map.elems) counterEvs
    remEvt <- mapDyn (leftmost . map snd . Map.elems) counterEvs
    return $ leftmost [ fmap (const Insert) (_el_clicked insert)
                      , fmap (uncurry Modify) (switchPromptlyDyn modEvt)
                      , fmap (Remove . fst) (switchPromptlyDyn remEvt) ]

main = mainWidget $ do
  rec changes <- view model
      model <- foldDyn update initModel changes
  return ()
