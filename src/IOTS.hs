{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module IOTS
  ( HasReps(typeReps)
  , MyTypeable
  , render
  , export
  ) where

import           Control.Monad.State          (State, evalState, gets, modify)
import           Data.Foldable                (fold, toList)
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as NEL
import           Data.Map                     (Map)
import           Data.Proxy                   (Proxy (Proxy))
import           Data.Sequence                (Seq, (<|), (|>))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Data.Tree                    (Tree (Node), rootLabel,
                                               subForest)
import           GHC.Exts                     (RuntimeRep, TYPE)
import           GHC.Generics                 ((:*:) ((:*:)), (:+:), C1,
                                               Constructor, D1, Datatype,
                                               Generic, M1 (M1), Rec0, Rep, S1,
                                               Selector, U1, conIsRecord,
                                               conName, selName)
import qualified GHC.Generics                 as Generics
import           IOTS.Leijen                  (jsArray, jsObject, jsParams,
                                               lowerFirst, render, stringDoc,
                                               upperFirst)
import           IOTS.Tree                    (depthfirstM)
import           Text.PrettyPrint.Leijen.Text (Doc, angles, braces, dquotes,
                                               linebreak, parens, punctuate,
                                               space, squotes, textStrict, vsep,
                                               (<+>))
import           Type.Reflection              (SomeTypeRep (SomeTypeRep),
                                               Typeable, someTypeRep)
import qualified Type.Reflection              as R

{-# ANN module ("HLint: ignore Avoid restricted function" :: Text)
         #-}

preamble :: Doc
preamble = "import * as t from 'io-ts'"

export ::
     forall a. (MyTypeable a)
  => a
  -> Doc
export _ = vsep . punctuate linebreak . toList $ preamble <| definitions
  where
    definitions :: Seq Doc
    definitions =
      flip evalState mempty . depthfirstM appendUnseenDefinitions mempty $
      flip evalState mempty . treeWalk $ myTypeRep @a
    appendUnseenDefinitions ::
         Seq Doc -> Iots -> State (Set SomeTypeRep) (Seq Doc)
    appendUnseenDefinitions acc Iots {..} = do
      seen <- gets (Set.member iotsRep)
      modify (Set.insert iotsRep)
      pure $
        if iotsOutput && not seen
          then acc |> iotsRef
          else acc

------------------------------------------------------------
iotsReps :: forall a. MyTypeRep a -> State Visited (NonEmpty (Tree Iots))
iotsReps Atom          = pure <$> typeReps @a
iotsReps (Fun from to) = (<>) <$> iotsReps from <*> iotsReps to

treeWalk :: forall a. MyTypeRep a -> State Visited (Tree Iots)
treeWalk Atom = typeReps @a
treeWalk rep@(Fun from to) = do
  lefts <- treeWalk from
  rights <- treeWalk to
  childReps <- iotsReps rep
  let children = lefts : subForest rights
      inputArguments = NEL.init childReps
      outputArgument = NEL.last childReps
      functionBinding = textStrict (upperFirst "someFunction") -- TODO Wrong
      boundParameters = withParameterLabels boundParameter
      labelledParameters = withParameterLabels labelledParameter
      boundParameter name r =
        "const" <+> toBinding name <+> "=" <+> toRef r <> ";"
      labelledParameter argName _ =
        textStrict (lowerFirst argName) <> ":" <+> typeof argName
      typeof argName = "t.TypeOf" <> angles ("typeof" <+> toBinding argName)
      toBinding argName = functionBinding <> "Arg" <> textStrict argName
      typeSignature =
        (if null inputArguments
           then mempty
           else jsParams (labelledParameters inputArguments) <> space) <>
        "=>" <+>
        typeof "Return"
      iotsRep = someTypeRep (Proxy @a)
      iotsOutput = True
      iotsRef =
        vsep $
        punctuate linebreak $
        fold
          [ boundParameters inputArguments
          , [boundParameter "Return" outputArgument]
          , ["type" <+> functionBinding <+> "=" <+> typeSignature <> ";"]
          ]
  pure $ Node (Iots {..}) children

withParameterLabels :: (Text -> Tree Iots -> Doc) -> [Tree Iots] -> [Doc]
withParameterLabels f = zipWith f (Text.singleton <$> ['A' .. 'Z'])

------------------------------------------------------------
-- Create our own universe of types.
data MyTypeRep (a :: k) where
  Atom
    :: forall (a :: *). (Typeable a, HasReps a)
    => MyTypeRep a
  Fun
    :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep) (a :: TYPE r1) (b :: TYPE r2).
       Typeable (a -> b)
    => MyTypeRep a
    -> MyTypeRep b
    -> MyTypeRep (a -> b)

------------------------------------------------------------
class MyTypeable a where
  myTypeRep :: MyTypeRep a

instance forall (a :: *). (Typeable a, HasReps a) => MyTypeable a where
  myTypeRep = Atom @a

instance {-# OVERLAPPING #-} forall a b. ( MyTypeable a
                                         , MyTypeable b
                                         , Typeable (a -> b)
                             ) =>
                             MyTypeable ((->) a b) where
  myTypeRep = Fun myTypeRep myTypeRep

------------------------------------------------------------
class HasReps a where
  typeReps :: State Visited (Tree Iots)
  default typeReps :: (Typeable a, Generic a, GenericHasReps (Rep a)) =>
    State Visited (Tree Iots)
  typeReps =
    genericTypeReps (someTypeRep (Proxy @a)) $ Generics.from (undefined :: a)

instance HasReps Text where
  typeReps = pure $ Node (Iots {..}) []
    where
      iotsRep = someTypeRep (Proxy @Text)
      iotsOutput = False
      iotsRef = "t.string"

instance HasReps Char where
  typeReps = pure $ Node (Iots {..}) []
    where
      iotsRep = someTypeRep (Proxy @Char)
      iotsOutput = False
      iotsRef = "t.string"

instance HasReps Integer where
  typeReps = pure $ Node (Iots {..}) []
    where
      iotsRep = someTypeRep (Proxy @Integer)
      iotsOutput = False
      iotsRef = "t.Int"

instance HasReps Int where
  typeReps = pure $ Node (Iots {..}) []
    where
      iotsRep = someTypeRep (Proxy @Int)
      iotsOutput = False
      iotsRef = "t.Int"

instance HasReps a => HasReps (Proxy a) where
  typeReps = typeReps @a

instance (HasReps a, HasReps b, Typeable a, Typeable b) => HasReps (a, b) where
  typeReps = do
    leftReps <- typeReps @a
    rightReps <- typeReps @b
    let children = [leftReps, rightReps]
        iotsRep = someTypeRep (Proxy @(a, b))
        iotsOutput = False
        iotsRef = "t.tuple" <> parens (jsArray (toRef <$> children))
    pure $ Node (Iots {..}) children

instance (HasReps k, HasReps v, Typeable k, Typeable v) =>
         HasReps (Map k v) where
  typeReps = do
    keyReps <- typeReps @k
    valueReps <- typeReps @v
    let children = [keyReps, valueReps]
        iotsRep = someTypeRep (Proxy @(Map k v))
        iotsOutput = False
        iotsRef = "t.record" <> jsParams (toRef <$> children)
    pure $ Node (Iots {..}) children

instance HasReps () where
  typeReps = do
    let iotsRep = someTypeRep (Proxy @())
        iotsOutput = False
        iotsRef = "t.null"
    pure $ Node (Iots {..}) []

instance (HasReps a, Typeable a) => HasReps (Maybe a) where
  typeReps = do
    aChildren <- typeReps @a
    nothingChildren <- typeReps @()
    let children = [aChildren, nothingChildren]
        iotsRep = someTypeRep (Proxy @(Maybe a))
        iotsOutput = False
        iotsRef = "t.union" <> parens (jsArray (toRef <$> children))
    pure $ Node (Iots {..}) children

------------------------------------------------------------
type family IsSpecialListElement a where
  IsSpecialListElement Char = 'True
  IsSpecialListElement a = 'False

instance (ListTypeReps flag a, flag ~ IsSpecialListElement a) =>
         HasReps [a] where
  typeReps = listTypeReps @flag @a

class ListTypeReps flag a where
  listTypeReps :: State Visited (Tree Iots)

instance ListTypeReps 'True Char where
  listTypeReps = pure $ Node (Iots {..}) []
    where
      iotsRep = someTypeRep (Proxy @String)
      iotsOutput = False
      iotsRef = "t.string"

instance (HasReps a, Typeable a) => ListTypeReps 'False a where
  listTypeReps = do
    child <- typeReps @a
    let iotsRep = someTypeRep (Proxy @[a])
        iotsOutput = False
        iotsRef = "t.array" <> parens (toRef child)
    pure $ Node (Iots {..}) [child]

------------------------------------------------------------
class GenericHasReps f where
  genericTypeReps :: SomeTypeRep -> f a -> State Visited (Tree Iots)

instance (GenericToBody p, Datatype f) => GenericHasReps (D1 f p) where
  genericTypeReps rep d = do
    child <- genericToDef rep d
    let iotsRep = rep
        iotsOutput = False
        iotsRef = repName rep
    pure $ Node (Iots {..}) [child]

data Cardinality
  = SoleConstructor
  | ManyConstructors

class GenericToDef f where
  genericToDef :: SomeTypeRep -> f a -> State Visited (Tree Iots)

instance (GenericToBody p, Datatype f) => GenericToDef (D1 f p) where
  genericToDef rep datatype@(M1 constructors) = do
    (childDef, childRefs) <- genericToBody rep constructors
    let moduleName = stringDoc $ Generics.moduleName datatype
        datatypeName = stringDoc $ Generics.datatypeName datatype
        ref = repName rep
        iotsRep = rep
        iotsOutput = True
        iotsRef =
          vsep
            [ "//" <+> moduleName <> "." <> datatypeName
            , "const" <+> ref <+> "=" <+> body <> ";"
            ]
        body =
          case childDef of
            [x] -> x SoleConstructor
            xs -> "t.union" <> parens (jsArray (($ ManyConstructors) <$> xs))
    pure $ Node (Iots {..}) childRefs

class GenericToBody f where
  genericToBody ::
       SomeTypeRep -> f a -> State Visited ([Cardinality -> Doc], [Tree Iots])

instance (Constructor f, GenericToFields p) => GenericToBody (C1 f p) where
  genericToBody rep constructor@(M1 selectors) = do
    (fieldBodies, children) <- genericToFields rep selectors
    let constructorName = stringDoc $ conName constructor
        def SoleConstructor =
          case (conIsRecord constructor, fieldBodies) of
            (False, []) -> "t.literal" <> parens (squotes constructorName)
            (False, [fieldBody]) -> fieldBody
            (False, _) -> "t.tuple" <> parens (jsArray fieldBodies)
            (True, _) -> "t.type" <> parens (jsObject fieldBodies)
        def ManyConstructors =
          case (conIsRecord constructor, fieldBodies) of
            (False, []) -> def SoleConstructor
            _           -> withNamedConstructor (def SoleConstructor)
        withNamedConstructor doc =
          "t.type" <> parens (braces (dquotes constructorName <> ":" <+> doc))
    pure ([def], children)

instance (GenericToBody f, GenericToBody g) => GenericToBody (f :+: g) where
  genericToBody rep _ =
    mappend <$> genericToBody rep (undefined :: f a) <*>
    genericToBody rep (undefined :: g a)

class GenericToFields f where
  genericToFields :: SomeTypeRep -> f a -> State Visited ([Doc], [Tree Iots])

instance GenericToFields U1 where
  genericToFields _ _ = pure mempty

instance (Selector s, HasReps p, Typeable p) =>
         GenericToFields (S1 s (Rec0 p)) where
  genericToFields _ selector = do
    let childRep = someTypeRep (Proxy @p)
    seen <- gets (Set.member childRep)
    modify (Set.insert childRep)
    child <- typeReps @p
    let fieldRef = toRef child
        def =
          case selName selector of
            ""   -> fieldRef
            name -> stringDoc name <> ":" <+> fieldRef
    pure $
      if seen
        then ([def], [])
        else ([def], [child])

instance (GenericToFields f, GenericToFields g) =>
         GenericToFields (f :*: g) where
  genericToFields rep ~(f :*: g) =
    (<>) <$> genericToFields rep f <*> genericToFields rep g

------------------------------------------------------------
type Visited = Set SomeTypeRep

data Iots =
  Iots
    { iotsRep    :: SomeTypeRep
        -- ^ The type this record describes.
    , iotsRef    :: Doc
        -- ^ How we refer to this type. For IOTS builtins this is
        -- probably `t.something`. For our definitions it will the the
        -- type name (eg. `User`).
    , iotsOutput :: Bool
        -- ^ Should we write this type out in the final export?
    }
  deriving (Show)

repName :: SomeTypeRep -> Doc
repName = stringDoc . fold . go
  where
    go :: SomeTypeRep -> [String]
    go rep@(SomeTypeRep someRep)
      | rep == R.someTypeRep (Proxy @String) = ["String"]
      | otherwise = headName : foldMap go params
      where
        (tyCon, params) = R.splitApps someRep
        headName =
          case R.tyConName tyCon of
            "[]"  -> "List"
            other -> other

toRef :: Tree Iots -> Doc
toRef = iotsRef . rootLabel
