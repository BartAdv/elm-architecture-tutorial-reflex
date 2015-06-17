{-# LANGUAGE RecursiveDo #-}
module CounterPair where

import Reflex.Dom
import qualified Counter

data Model = Model { topCounter :: Counter.Model
                   , bottomCounter :: Counter.Model }

initCounter :: Int -> Int -> Model
initCounter = Model

data Action
  = Reset
  | Top Counter.Action
  | Bottom Counter.Action

update :: Action -> Model -> Model
update Reset model = initCounter 0 0
update (Top act) model = model { topCounter = Counter.update act (topCounter model) }
update (Bottom act) model = model { bottomCounter = Counter.update act (bottomCounter model) }

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model =
  el "div" $ do
    tc <- mapDyn topCounter model
    bc <- mapDyn bottomCounter model
    topAction <- Counter.view tc
    bottomAction <- Counter.view bc
    (btn, _) <- el' "button" $ text "RESET"
    return $ leftmost [ fmap Top topAction
                      , fmap Bottom bottomAction
                      , fmap (const Reset) (_el_clicked btn)]

main = mainWidget $ do
  let initial = initCounter 0 0
  rec changes <- view model
      model <- foldDyn update initial changes
  return ()
