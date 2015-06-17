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
  | Remove
  | Modify ID Counter.Action

update :: Action -> Model -> Model
update Insert model =
  let newCounter = (nextID model, Counter.initCounter 0)
      newCounters = (counters model) ++ [ newCounter ]
  in model { counters = newCounters
           , nextID = (nextID model) + 1 }
update Remove model = model { counters = drop 1 (counters model) }
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
    (remove, _) <- el' "button" $ text "Remove"
    (insert, _) <- el' "button" $ text "Add"
    counters <- mapDyn (Map.fromList . counters) model
    -- we enhance the normal counter event with the id
    counterEvs <- listWithKey counters (\k m -> do
                                           ev <- Counter.view m
                                           return $ fmap (k,) ev)
    -- because we follow Elm example, let's simplify here and choose first event
    modEvt <- mapDyn (leftmost . Map.elems) counterEvs
    return $ leftmost [ fmap (const Remove) (_el_clicked remove)
                      , fmap (const Insert) (_el_clicked insert)
                      , fmap (uncurry Modify) $ switchPromptlyDyn modEvt]

main = mainWidget $ do
  rec changes <- view model
      model <- foldDyn update initModel changes
  return ()
