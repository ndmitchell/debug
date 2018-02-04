module Debug where

import Data.Foreign
import Data.Foreign.Index
import Data.Foreign.Keys
import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, class Plus, empty)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.JQuery (JQuery, clear, create, on, ready, select)
import Control.Monad.Eff.JQuery as JQuery
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (ExceptT, except, lift, mapExceptT, runExcept, runExceptT, throwError)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.MonadPlus (class MonadZero, guard)
import DOM (DOM)
import Data.Array (init, sortWith, unsafeIndex, (!!), (..), (:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either, either, fromRight)
import Data.Foldable (findMap)
import Data.Foreign.Class (class Decode, decode)
import Data.Identity (Identity(..))
import Data.List (List(..), foldM, reverse)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (class Newtype, over, un)
import Data.Set (fromFoldable)
import Data.String (Pattern(..), drop, indexOf, joinWith)
import Data.String as String
import Data.String.Regex (Regex, match, regex, test)
import Data.String.Regex.Flags (global, noFlags)
import Data.Traversable (class Traversable, for, for_, sequence)
import Partial.Unsafe (unsafePartial, unsafePartialBecause)

--------------------------------------------------
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

instance callDataShow :: Show CallData where
  show cdata = either show id $ runE $ renderCall cdata

getArg :: String -> CallData -> Maybe String
getArg k (CallData f) = either (const Nothing) Just $ runE $ do
  ix <- liftF f k $ decode =<< index f k
  fromMaybeE (VariableIndexError ix) $ trace.variables !! ix

callDepends :: forall m . Monad m => CallData -> E m (Array {ix::Int,val::CallData})
callDepends (CallData f)
  | hasProperty "$depends" f = do
    ixes <- liftF f "depends" $ decode =<< index f "$depends"
    for ixes $ \ix -> do
                      val <- fromMaybeE (CallIndexError ix) $ trace.calls !! ix
                      pure {ix: ix, val: val}
  | otherwise = pure []

callParents :: forall m . Monad m => CallData -> E m (Array {ix::Int,val::CallData})
callParents (CallData f)
  | hasProperty "$parents" f = do
    ixes <- liftF f "parents" $ decode =<< index f "$parents"
    for ixes $ \ix -> do
                      val <- fromMaybeE (CallIndexError ix) $ trace.calls !! ix
                      pure {ix: ix, val: val}
  | otherwise = pure []

callVals :: forall m . Monad m => CallData -> E m (Array {name::String, value::String})
callVals (CallData f) = do
  kk <- liftF f "call data" $ keys f
  embedE $ do
    k <- lift kk
    lift $ guard (k /= "" && k /= "$parents" && k /= "$depends" )
    kv <- liftF f ("call " <> k) $ f!k
    ix <- decodeE k kv
    arg <- maybe (throwE $ VariableIndexError ix) pure
           $ trace.variables !! ix
    pure {name: k, value: arg}

callFunction :: forall m . Monad m => CallData -> E m DebugFunction
callFunction (CallData f) = do
  funId <- liftF f "call function" $ readInt =<< index f ""
  case trace.functions !! funId of
    Just x -> pure x
    Nothing -> throwE (FunctionIndexError funId)

-- | Returns the call stack upwards from the given call
getCallStack :: forall m . Monad m => {ix::Int,val::CallData} -> E m (List {ix::Int,val::CallData})
getCallStack x = do
  pars <- callParents x.val
  case pars of
    [p] -> (Cons p) <$> getCallStack p
    []  -> pure Nil
    _   -> throwError (singleton $ MoreThanOneParent x.ix)

renderCall :: forall m . Monad m => CallData -> E m String
renderCall c = do
  fun  <- callFunction c
  vals <- callVals c
  let findArg arg = fromMaybe "_"
                  $ findMap (\val -> guard(val.name == arg) *> pure val.value) vals
  pure $ joinWith " " $
    fun.name : map findArg fun.arguments <> [" = ", findArg fun.result]

matchCalls :: forall m . Monad m =>
              String -> String -> E m (Array {index::Int, rendered::String} )
matchCalls name regexp = embedE $ do
  let nameMatch
        | name == "(All)" = \_ -> pure true
        | otherwise = \c -> do
          fun <- callFunction c
          pure (name == fun.name)
      regexpMatch :: String -> Boolean
      regexpMatch = either (\_ _ -> false) test $ regex regexp noFlags
  i  <- lift $ unsafePartial $ fromJust $ init $ 0 .. (Array.length trace.calls)
  let c = unsafePartial $ unsafeIndex trace.calls i
  nc <- nameMatch c
  guard nc
  rc <- renderCall c
  guard (regexpMatch rc)
  pure {index:i, rendered:rc}

-----------------------------------------------------------------------
-- DOM manipulation

main :: forall eff . Eff (console :: CONSOLE, dom :: DOM | eff) Unit
main = ready $ do
  let funcNames = fromFoldable $ map (_.name) trace.functions
  drop <- select "#function-drop"
  text <- select "#function-text"
  for_ funcNames $ \n -> do
    opt <- create $ "<option>" <> escapeHTML n <> "</option>"
    JQuery.append opt drop

  let showCalls = attempt $ do
        name    <- decodeE "name"   =<< lift (JQuery.getValue drop)
        regexp  <- decodeE "regexp" =<< lift (JQuery.getValue text)
        matches <- matchCalls name regexp
        list    <- lift $ select "#function-list"
        lift $ clear list
        for_ matches $ \x -> lift $ do
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

showSource :: forall eff. JQuery -> CallData -> E (Eff (dom :: DOM | eff)) Unit
showSource dom c = do
  lift $ clear dom
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
  lift $ loop fun.source

showCall :: forall eff. Int -> Eff (console :: CONSOLE, dom :: DOM | eff) Unit
showCall n = attempt $ do
  c   <- fromMaybeE (CallIndexError n) $ trace.calls !! n
  fun <- callFunction c

  source <- lift $ select "#function-source"
  showSource source c

  variables <- lift $ select "#function-variables"
  lift $ clear variables
  vals <- callVals c
  lift $ for_ (sortWith (_.name) vals) \v -> do
    el <- create $ "<li><pre>" <> v.name <> " = " <> escapeHTML v.value <> "</pre></li>"
    JQuery.append el variables

  deps <- callDepends c
  pars <- callParents c
  callstackSection <- lift $ select "#function-depends-section"
  cursor <- lift $ select "#function-depends"
  lift $ clear cursor
  lift $ JQuery.hide callstackSection
  when (Array.length deps > 0 || Array.length pars > 0) $ do
    let this = {ix:n,val:c}
    callStack <- reverse <$> getCallStack this
    cursor <- foldM mkCallStackLinkEntry cursor callStack
    cursor <- mkCallStackEntry cursor =<< renderCall this.val
    for_ deps (mkCallStackLinkEntry cursor)
    lift $ JQuery.display callstackSection

-- | Creates the html string for a call link:
--     <a href="javascript:showCall(ix)>renderedCall</a>"
-- NOTE Works with pulp build, but not with pulp browserify...
mkCallLink :: { index :: Int, rendered :: String} -> String
mkCallLink x =
  "<a href='javascript:PS.Debug.showCall(" <> show x.index <> ")()'>" <> escapeHTML x.rendered <> "</a>"

mkCallStackLinkEntry :: forall m eff . JQuery -> {ix::Int,val::CallData} -> E (Eff (dom :: DOM | eff)) JQuery
mkCallStackLinkEntry parent x = do
  r <- renderCall x.val
  mkCallStackEntry parent (mkCallLink {index:x.ix, rendered:r})
mkCallStackEntry :: forall m eff . JQuery -> String -> E (Eff (dom :: DOM | eff)) JQuery
mkCallStackEntry parent contents = do
  temp <- lift $ create "<ul>"
  newEntry <- lift $ create $ "<li>" <> contents <> "</li>"
  lift $ JQuery.append newEntry temp
  lift $ JQuery.append temp parent
  pure newEntry

--------------------------------------------------
-- Our error handling monad

newtype E m a = E (ExceptT (NonEmptyList MyError) m a)

derive instance newtypeE :: Newtype (E m a) _
derive newtype instance functorE :: Functor m => Functor (E m)
derive newtype instance applyE :: Monad m => Apply (E m)
derive newtype instance applicativeE :: Monad m => Applicative (E m)
derive newtype instance altE :: Monad m => Alt (E m)
derive newtype instance bindE :: Monad m => Bind (E m)
derive newtype instance monadE :: Monad m => Monad (E m)
derive newtype instance monadTransE :: MonadTrans E
derive newtype instance monadThrowE :: Monad m => MonadThrow (NonEmptyList MyError) (E m)

runE :: forall a. E Identity a -> Either (NonEmptyList MyError) a
runE = un E >>> runExcept
runE' :: forall a m. E m a -> m (Either (NonEmptyList MyError) a)
runE' = un E >>> runExceptT

throwE :: forall m a. Monad m => MyError -> E m a
throwE = throwError <<< singleton

-- Custom instances for backtracking
instance plusE :: (Alternative m, Monad m) => Plus (E m) where empty = lift empty
instance alternativeE :: (Alternative m, Monad m) => Alternative (E m)
instance monadZeroE :: (MonadZero m) => MonadZero (E m)

liftF :: forall a m . Applicative m => Foreign -> String -> F a -> E m a
liftF f part = E <<< mapExceptT (un Identity >>> lmap (map (TraceError part f)) >>> pure)

decodeE :: forall m a . Decode a => Applicative m => String -> Foreign -> E m a
decodeE part f = liftF f part $ decode f

embedE :: forall a m n . Monad m => Traversable n => E n a -> E m (n a)
embedE = over E $ runExceptT >>> map except >>> sequence

fromMaybeE :: forall m a . Monad m => MyError -> Maybe a -> E m a
fromMaybeE msg = maybe (throwE msg) pure

attempt :: forall eff . E (Eff (console :: CONSOLE | eff)) Unit -> Eff (console :: CONSOLE | eff) Unit
attempt e = runE' e >>= either logShow pure

-- | Our error type
data MyError
  = TraceError String Foreign ForeignError
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
