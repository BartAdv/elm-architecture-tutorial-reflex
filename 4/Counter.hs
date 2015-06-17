{-# LANGUAGE RecursiveDo #-}
module Counter (Model, initCounter, Action, update, view, viewWithRemoveButton) where

import Reflex.Dom

type Model = Int

initCounter :: Int -> Model
initCounter count = count

data Action = Increment | Decrement

update :: Action -> Model -> Model
update Increment model = model + 1
update Decrement model = model - 1

-- we do not return html, it's done by MonadWidget
view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model =
  el "div" $ do
    (decrement, _) <- el' "button" $ text "-"
    el "div" $ do
      t <- mapDyn show model
      dynText t
    (increment, _) <- el' "button" $ text "+"
    return $ leftmost [ fmap (const Decrement) (_el_clicked decrement)
                      , fmap (const Increment) (_el_clicked increment) ]

viewWithRemoveButton :: MonadWidget t m => Dynamic t Model -> m (Event t Action, Event t ())
viewWithRemoveButton model =
  el "div" $ do
    modEv <- view model
    (remove, _) <- el' "button" $ text "X"
    return $ ( modEv
             , fmap (const ()) (_el_clicked remove) )
