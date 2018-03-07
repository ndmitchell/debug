-- Not currently used
-- Built using
-- > pulp build -m Debug -O --to ../html/debug.js

module Debug where

import Data.Foreign
import Data.Foreign.Index
import Data.Foreign.Keys
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.JQuery (JQuery, clear, create, on, ready, select)
import Control.Monad.Eff.JQuery as JQuery
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import DOM (DOM)
import Data.Array (init, sortWith, unsafeIndex, (!!), (..), (:))
import Data.Array as Array
import Data.Either (either, fromRight)
import Data.Foldable (findMap)
import Data.Foreign.Class (class Decode, decode)
import Data.List (List(..), foldM, reverse)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Set (fromFoldable)
import Data.String (Pattern(..), drop, indexOf, joinWith)
import Data.String as String
import Data.String.Regex (Regex, match, regex, test)
import Data.String.Regex.Flags (global, noFlags)
import Data.Traversable (for, for_)
import Partial.Unsafe (unsafePartial)

-- Foreign imports

-- | The debugging trace
foreign import trace :: DebugTrace
-- | Escape strings for embedding in HTML
foreign import escapeHTML :: String -> String

----------------------------------------------------------------
-- Purescript versions of the Haskell debug package types

type DebugFunction =
    {name :: String -- ^ Function name
    ,source :: String -- ^ Function source, using @\n@ to break lines
    ,arguments :: Array String -- ^ Variables for the arguments to the function
    ,result :: String -- ^ Variable for the result of the function
    }

type DebugTrace =
  { functions :: Array DebugFunction  -- ^ Flat list of all the functions traced
  , variables :: Array String    -- ^ Flat list of all the variable values observed
  , calls     :: Array CallData  -- ^ Flat list of all the function calls traced
  }

newtype CallData = CallData Foreign

getArg :: String -> CallData -> Maybe String
getArg k (CallData f) = join $ either (const Nothing) Just $ runExcept $ do
  ix <- decode =<< index f k
  pure $ trace.variables !! ix

callDepends :: forall m . CallData -> E m (Array {ix::Int,val::CallData})
callDepends (CallData f)
  | hasProperty "$depends" f = do
    ixes <- liftF f "depends" $ decode =<< index f "$depends"
    for ixes $ \ix -> do
                      val <- fromMaybeE (CallIndexError ix) $ trace.calls !! ix
                      pure {ix: ix, val: val}
  | otherwise = pure []

callParents :: forall m . CallData -> E m (Array {ix::Int,val::CallData})
callParents (CallData f)
  | hasProperty "$parents" f = do
    ixes <- liftF f "parents" $ decode =<< index f "$parents"
    for ixes $ \ix -> do
                      val <- fromMaybeE (CallIndexError ix) $ trace.calls !! ix
                      pure {ix: ix, val: val}
  | otherwise = pure []

callVals :: forall m . CallData -> E m (Array {name::String, value::String})
callVals (CallData f) = do
  kk <- liftF f "call data" $ keys f
  let val_kks = Array.filter (\k -> k /= "" && k /= "$parents" && k /= "$depends" ) kk
  for val_kks $ \k -> do
    kv <- liftF f ("call " <> k) $ f!k
    ix <- decodeE k kv
    arg <- maybe (throwE $ VariableIndexError ix) pure
           $ trace.variables !! ix
    pure {name: k, value: arg}

callFunction :: forall m . CallData -> E m DebugFunction
callFunction (CallData f) = do
  funId <- liftF f "call function" $ readInt =<< index f ""
  case trace.functions !! funId of
    Just x -> pure x
    Nothing -> throwE (FunctionIndexError funId)

-- | Returns the call stack upwards from the given call
getCallStack :: forall m . {ix::Int,val::CallData} -> E m (List {ix::Int,val::CallData})
getCallStack x = do
  pars <- callParents x.val
  case pars of
    [p] -> (Cons p) <$> getCallStack p
    []  -> pure Nil
    _   -> throwE (MoreThanOneParent x.ix)

renderCall :: forall m . CallData -> E m String
renderCall c = do
  fun  <- callFunction c
  vals <- callVals c
  let findArg arg = fromMaybe "_"
                  $ findMap (\val -> guard(val.name == arg) *> pure val.value) vals
  pure $ joinWith " " $
    fun.name : map findArg fun.arguments <> [" = ", findArg fun.result]

matchCalls :: forall m .
              String -> String -> E m (Array {index::Int, rendered::String} )
matchCalls name regexp = do
  let nameMatch
        | name == "(All)" = \_ -> pure true
        | otherwise = \c -> do
          fun <- callFunction c
          pure (name == fun.name)
      regexpMatch :: String -> Boolean
      regexpMatch = either (\_ _ -> false) test $ regex regexp noFlags
  let indexes = unsafePartial $ fromJust $ init $ 0 .. (Array.length trace.calls)
  map Array.catMaybes $ for indexes $ \i -> do
    let c = unsafePartial $ unsafeIndex trace.calls i
    ifM (nameMatch c)
      (do rc <- renderCall c
          pure $ if (regexpMatch rc) then Just {index:i, rendered:rc} else Nothing)
      (pure Nothing)

-----------------------------------------------------------------------
-- DOM manipulation

main :: forall eff . Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
main = ready $ do
  let funcNames = fromFoldable $ map (_.name) trace.functions
  drop <- select "#function-drop"
  text <- select "#function-text"
  for_ funcNames $ \n -> do
    opt <- create $ "<option>" <> escapeHTML n <> "</option>"
    JQuery.append opt drop

  let showCalls = attempt $ do
        name    <- decodeE "name"   =<< JQuery.getValue drop
        regexp  <- decodeE "regexp" =<< JQuery.getValue text
        matches <- matchCalls name regexp
        list    <- select "#function-list"
        clear list
        for_ matches $ \x -> do
          li <- create $ "<li>" <> mkCallLink x <> "</li>"
          JQuery.append li list

  on "change" (\_ _ -> showCalls) drop
  on "change" (\_ _ -> showCalls) text
  on "input"  (\_ _ -> showCalls) text

  showCalls
  showCall 0

hsLexer :: Regex
hsLexer = unsafePartial fromRight
          $ regex """[a-zA-Z][a-zA-Z0-9]+|[\r\n]|.""" global
symbols :: String
symbols = """->:=()[]"""

showSource :: forall eff. JQuery -> CallData -> E (dom :: DOM | eff) Unit
showSource dom c = do
  clear dom
  fun <- callFunction c
  let loop src = do
        case match hsLexer src of
          Just mm | Just (Just m) <- mm!!0, m /= "" -> do
             case m of
               "where" -> do
                 it <- create "<span class='hs-keyword'>where</span>"
                 JQuery.append it dom
               _ | Just _ <- indexOf (Pattern m) symbols -> do
                 it <- create $ "<span class='hs-keyglyph'>" <> escapeHTML m <> "</a>"
                 JQuery.append it dom
               _ | Just arg <- getArg m c -> do
                 it <- create $ "<abbr title='" <> escapeHTML arg <> "'>" <> escapeHTML m <> "</abbr>"
                 JQuery.append it dom
               _ ->
                 JQuery.appendText m dom
             loop $ drop (String.length m) src
          _ ->
            JQuery.appendText src dom
  loop fun.source

showCall :: forall eff. Int -> Eff (exception :: EXCEPTION, dom :: DOM | eff) Unit
showCall n = attempt $ do
  c   <- fromMaybeE (CallIndexError n) $ trace.calls !! n
  fun <- callFunction c

  source <- select "#function-source"
  showSource source c

  variables <- select "#function-variables"
  clear variables
  vals <- callVals c
  for_ (sortWith (_.name) vals) \v -> do
    el <- create $ "<li><pre>" <> v.name <> " = " <> escapeHTML v.value <> "</pre></li>"
    JQuery.append el variables

  deps <- callDepends c
  pars <- callParents c
  callstackSection <- select "#function-depends-section"
  cursor <- select "#function-depends"
  clear cursor
  JQuery.hide callstackSection
  when (Array.length deps > 0 || Array.length pars > 0) $ do
    let this = {ix:n,val:c}
    callStack <- reverse <$> getCallStack this
    cursor <- foldM mkCallStackLinkEntry cursor callStack
    cursor <- mkCallStackEntry cursor =<< renderCall this.val
    for_ deps (mkCallStackLinkEntry cursor)
    JQuery.display callstackSection

-- | Creates the html string for a call link:
--     <a href="javascript:showCall(ix)>renderedCall</a>"
-- NOTE Works with pulp build, but not with pulp browserify...
mkCallLink :: { index :: Int, rendered :: String} -> String
mkCallLink x =
  "<a href='javascript:PS.Debug.showCall(" <> show x.index <> ")()'>" <> escapeHTML x.rendered <> "</a>"

mkCallStackLinkEntry :: forall m eff . JQuery -> {ix::Int,val::CallData} -> E (dom :: DOM | eff) JQuery
mkCallStackLinkEntry parent x = do
  r <- renderCall x.val
  mkCallStackEntry parent (mkCallLink {index:x.ix, rendered:r})

mkCallStackEntry :: forall m eff . JQuery -> String -> E ((dom :: DOM | eff)) JQuery
mkCallStackEntry parent contents = do
  temp <- create "<ul>"
  newEntry <- create $ "<li>" <> contents <> "</li>"
  JQuery.append newEntry temp
  JQuery.append temp parent
  pure newEntry

--------------------------------------------------
-- Our error handling monad

type E eff = Eff (exception :: EXCEPTION | eff)

runE :: forall eff a. E eff a -> Eff (exception :: EXCEPTION | eff) a
runE = id

throwE :: forall m a. MyError -> E m a
throwE e = throwException (error $ show e)

liftF :: forall a m . Foreign -> String -> F a -> E m a
liftF f part = either (throwE <<< TraceError part f) pure <<< runExcept

decodeE :: forall m a . Decode a => String -> Foreign -> E m a
decodeE part f = liftF f part $ decode f

fromMaybeE :: forall m a . MyError -> Maybe a -> E m a
fromMaybeE msg = maybe (throwE msg) pure

attempt :: forall eff . E eff Unit -> Eff (exception :: EXCEPTION | eff) Unit
attempt = id

-- | Our error type
data MyError
  = TraceError String Foreign (NonEmptyList ForeignError)
  | FunctionIndexError Int
  | VariableIndexError Int
  | CallIndexError Int
  | MoreThanOneParent Int

instance showMyError :: Show MyError where
  show (TraceError part obj f) =
    "Error when trying to decode trace " <> part <> ": " <> show f <> "\n\n" <> tagOf obj
  show (FunctionIndexError ix) = "Error when trying to index the Functions array: " <> show ix
  show (VariableIndexError ix) = "Error when trying to index the Variables array: " <> show ix
  show (CallIndexError ix) = "Error when trying to index the Calls array: " <> show ix
  show (MoreThanOneParent ix) = "Error computing the call stack, more than one parent: " <> show ix
