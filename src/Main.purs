module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import Data.Maybe (Maybe, fromJust)
import DOM.Node.ParentNode (querySelector, QuerySelector(..)) as DOM
import React as R
import React.DOM as R
import React (createFactory) as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite  as T
import Partial.Unsafe (unsafePartial)

type State = Int

data Action = Increment | Decrement

initialState :: State
initialState = 42

render :: T.Render State _ _
render dispatch _ state _ =
  [ R.h1' [ R.text "Lesson 2 - Actions" ]
  , R.p'  [ R.text "The state is: "
          , R.text (show state) ]
  , R.p   [ RP.className "btn-group" ]
          [ R.button [ RP.className "btn btn-success"
                     , RP.onClick \_ -> dispatch Increment ]
                     [ R.text "Increment" ]
          , R.button [ RP.className "btn btn-danger"
                     , RP.onClick \_ -> dispatch Decrement ]
                     [ R.text "Decrement" ]
          ]
  ]

performAction :: T.PerformAction _ State _ Action
performAction Increment _ _ = void $ T.modifyState $ \state -> state + 1
performAction Decrement _ _ = void $ T.modifyState $ \state -> state - 1

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

-- main :: forall a b. b -> Eff (dom :: DOM | a) ()
main :: forall eff. Eff (dom :: DOM | eff) (Maybe R.ReactComponent)
main = unsafePartial do
  let component = T.createClass spec initialState
  document <- DOM.window >>= DOM.document
  container <- fromJust <$> DOM.querySelector (DOM.QuerySelector "#container") (DOM.htmlDocumentToParentNode document)
  RDOM.render (R.createFactory component {}) container
