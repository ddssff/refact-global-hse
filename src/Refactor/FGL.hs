-- | This is a state monad containing a 'NodeMap', with basic
-- operations on the simple Patricia tree graph implementation.  It
-- helps to avoid some pitfalls of the "bare" graph implementation,
-- such as replacing existing nodes with new identical ones.

{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, StandaloneDeriving #-}

module Refactor.FGL
    ( -- * Pure operations
      labNodes, labEdges, mkGraph
      -- * With NodeMap state
    , runGraphT, runGraph
    , evalGraphT, evalGraph
    , mkNode
    , mkGraphM
    , insEdge, insNode
    , delEdge
    , labNode
    , labEdgesM
    , out
    , context
    , efilter
    , reachable
    , components
    -- * return pairs
    , labNode', labNodes', labEdges'
    -- * Tests
    , tests
    ) where

import Control.Lens hiding (elements)
import Control.Monad.State
import Data.Default (Default(def))
import Data.Foldable (foldrM)
import qualified Data.Graph.Inductive as G
import Data.Map as Map (Map, filter, fromList, insert)
import Data.Set as Set (Set, fromList, null)
import Data.Tuple (swap)
import Test.HUnit

runGraphT :: Ord a => StateT (G.NodeMap a) m r -> m (r, G.NodeMap a)
runGraphT f = runStateT f G.new

runGraph :: Ord a => StateT (G.NodeMap a) Identity r -> (r, G.NodeMap a)
runGraph f = runState f G.new

evalGraphT :: (Ord a, Monad m) => StateT (G.NodeMap a) m r -> m r
evalGraphT f = evalStateT f G.new

evalGraph :: (G.DynGraph gr, Ord a) => (gr a b -> StateT (G.NodeMap a) Identity r) -> gr a b -> r
evalGraph f gr = evalState (f gr) (G.fromGraph gr)

-- | 'evalGraph' starting with an empty nodemap
evalGraph' :: Ord a => StateT (G.NodeMap a) Identity r -> r
evalGraph' f = evalState f G.new

-- | foldrM with arguments flipped
foldrM' :: (Foldable t, Monad m) => (a -> b -> m b) -> t a -> b -> m b
foldrM' f t r = foldrM f r t

-- | Build a graph from nodes and edges
mkGraphM :: forall m gr a b. (Monad m, G.DynGraph gr, Ord a) =>
            [a] -> [(a, a, b)] -> StateT (G.NodeMap a) m (gr a b)
mkGraphM ns es = foldrM' insNode ns G.empty >>= foldrM' insEdge es

instance Default (G.Gr a b) where
  def = G.empty

-- | Build a graph from nodes and edges
mkGraph :: forall gr a b. (G.DynGraph gr, Ord a) =>
           [a] -> [(a, a, b)] -> gr a b
mkGraph ns es = evalGraph' (mkGraphM ns es)

edgesFromGraph :: forall a b. (Ord a, Ord b) => G.Gr a b -> Map a (Set (b, a))
edgesFromGraph gr =
    Map.filter (not . Set.null) (foldl f mempty (G.nodes gr))
    where
      f :: Map a (Set (b, a)) -> G.Node -> Map a (Set (b, a))
      f mp n = Map.insert ln1 es' mp
          where
            c = (G.context gr n)
            ln1 :: a
            ln1 = snd (G.labNode' c)
            es :: [(G.Node, b)]
            es = (G.lsuc' c)
            es' :: Set (b, a)
            es' = Set.fromList (fmap (swap . over _1 g) es)
      g :: G.Node -> a
      g n = snd (G.labNode' (G.context gr n))

#if 0
graphFromEdges :: forall a b. (Ord a, Ord b) => Map a (Set (b, a)) -> (G.Gr a b, G.NodeMap a)
graphFromEdges mp =
    Map.foldlWithKey f (G.empty, G.new) mp
    where
      f :: (G.Gr a b, G.NodeMap a) -> a -> Set (b, a) -> (G.Gr a b, G.NodeMap a)
      f (gr, nm) n1 s =
          let (ln1, nm') = G.mkNode nm n1
              gr' = G.insNode ln1 gr in
          Set.fold (g ln1) (gr', nm') s
      g :: G.LNode a -> (b, a) -> (G.Gr a b, G.NodeMap a) -> (G.Gr a b, G.NodeMap a)
      g ln1 (e, n2) (gr, nm) =
          let (ln2, nm') = G.mkNode nm n2
              gr' = G.insNode ln2 gr
              gr'' = G.insEdge (fst ln1, fst ln2, e) gr' in
          (gr'', nm')
#endif

-- | Insert a node if necessary
insNode :: (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a) => a -> gr a b -> m (gr a b)
insNode t gr = do
  nm <- get
  let (ln1@(n1, _), nm') = G.mkNode nm t
  let gr' = if G.gelem n1 gr then gr else G.insNode ln1 gr
  put nm'
  return gr'

insEdge :: (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a) => (a, a, b) -> gr a b -> m (gr a b)
insEdge (t1, t2, e) gr = do
  gr' <- insNode t1 gr >>= insNode t2
  n1 <- mkNode t1
  n2 <- mkNode t2
  return $ G.insEdge (n1, n2, e) gr'

mkNode :: forall m a. (MonadState (G.NodeMap a) m, Ord a) => a -> m G.Node
mkNode t = do
  ((n, _), nm') <- G.mkNode <$> get <*> pure t
  put nm'
  return n

-- | Assumes the nodes and the edge exist
delEdge :: (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a, Eq b) => (a, a, b) -> gr a b -> m (gr a b)
delEdge (t1, t2, e) gr = do
  n1  <- mkNode t1
  n2  <- mkNode t2
  return $ G.delLEdge (n1, n2, e) gr

out ::
    forall gr a b m. (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a, Eq b)
    => gr a b
    -> a
    -> m [(a, a, b)]
out gr n = do
  n' <- mkNode n
  let edges :: [(G.Node, G.Node, b)]
      edges = G.out gr n'
  mapM (\(n1, n2, b) -> (,,) <$> labNode gr n1 <*> labNode gr n2 <*> pure b) edges

context ::
    forall gr a b m. (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a, Eq b)
    => gr a b
    -> a
    -> m ([(a, b)], a, [(b, a)])
context gr a = do
  n' <- mkNode a
  let (pre, n'', a', suc) = G.context gr n'
  pre' <- mapM (\(b, n''') -> (,) <$> labNode gr n''' <*> pure b) pre
  suc' <- mapM (\(b, n''') -> (,) <$> pure b <*> labNode gr n''') suc
  return $ (pre', a', suc')

labNode :: (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a) => gr a b -> G.Node -> m a
labNode gr n = do
  let c = G.context gr n
  let (_, t1) = G.labNode' c
  return t1

labNode' :: (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a) => gr a b -> G.Node -> m (G.Node, a)
labNode' gr n = (,) <$> pure n <*> labNode gr n

labNodes :: (G.DynGraph gr, Ord a) => gr a b -> [a]
labNodes gr = fmap snd $ G.labNodes gr

labNodes' :: (G.DynGraph gr, Ord a) => gr a b -> [G.LNode a]
labNodes' gr = G.labNodes gr

labEdgesM :: (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a) => gr a b -> m [(a, a, b)]
labEdgesM gr = mapM (\(n1, n2, b) -> (,,) <$> labNode gr n1 <*> labNode gr n2 <*> pure b) (G.labEdges gr)

labEdges :: (G.DynGraph gr, Ord a) => gr a b -> [(a, a, b)]
labEdges gr = evalState (labEdgesM gr) (G.fromGraph gr)

labEdgesM' :: forall gr a b m. (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a) => gr a b -> m [((G.Node, a), (G.Node, a), b)]
labEdgesM' gr =
    mapM (\(n1, n2, b) -> (,,) <$> labNode' gr n1 <*> labNode' gr n2 <*> pure b) (G.labEdges gr)

labEdges' :: (G.DynGraph gr, Ord a) => gr a b -> [((G.Node, a), (G.Node, a), b)]
labEdges' gr = evalState (labEdgesM' gr) (G.fromGraph gr)

-- | Keep edges that satisfy a predicate
efilter :: (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a, Eq b) =>
           ((a, a, b) -> Bool) -> gr a b -> m (gr a b)
efilter p gr0 = do
  foldrM (\(n1, n2, l) gr -> do
            t1 <- labNode gr n1
            t2 <- labNode gr n2
            let e' = (t1, t2, l)
            if p e' then pure gr else delEdge e' gr) gr0 (G.labEdges gr0)

reachable :: (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a) => gr a b -> a -> m (Set a)
reachable gr n = mkNode n >>= \n' -> Set.fromList <$> mapM (labNode gr) (G.reachable n' gr)

components :: forall gr a b m. (MonadState (G.NodeMap a) m, G.DynGraph gr, Ord a) => gr a b -> m [[a]]
components gr =
    (sequence . fmap (sequence . fmap (labNode gr)) $ G.components gr)

#if 0
instance Arbitrary Type where
    arbitrary = elements [TupleT 0, TupleT 2, TupleT 3, TupleT 4, ListT, ConT (mkName "C")]

instance Arbitrary (Hop ()) where
    arbitrary = elements [TupleHop 1,
                          TupleHop 2,
                          TupleHop 3,
                          TupleHop 4,
                          IndexHop ()]

instance Arbitrary GP where
    arbitrary = (GP . graphFromEdges) <$> arbitrary

type A = Char
type B = String

newtype GP = GP (G.Gr A B, G.NodeMap A) deriving Show
#endif

{-
  o <- G.out <$> use _1 <*> pure n1
  i <- G.inn <$> use _1 <*> pure n1
  let s = case Prelude.filter ((== n2) . view _2) (trace ("o(" ++ show n1 ++ ")=" ++ show o ++ ", i(" ++ show n1 ++ ")=" ++ show i) o) of
            [] -> mempty
            [x@(_, _, s)] -> trace ("s=" ++ show x) s
            _ -> error "multiset??"
  _1 %= (\(g :: G.Gr a b) -> trace (" after: " ++ show (G.insEdge (n1, n2, f s e) g))
                                   (G.insEdge (n1, n2, f s e) (trace ("before: " ++ show g) g)))

t1 :: Show a => a -> a
t1 x = trace ("before=" ++ show x) x

t2 :: Show a => a -> a
t2 x = trace (" after=" ++ show x) x
-}

tests :: Test
tests =
    TestList
    [ TestCase (assertEqual
                  "test1"
                  (Map.fromList [('a',Set.fromList [("ab",'b')])])
                  (evalGraph' (edgesFromGraph <$> mkGraphM ['a','b'] [('a','b',"ab")])))
    , TestCase (assertEqual
                  "test2"
                  (evalGraph' (mkGraphM ['a','b','c','d','e','f'] [('b','e',"be"), ('b','f',"bf"), ('b','f',"bx"), ('b','e',"by")]) :: G.Gr Char String) -- expected
                  (evalGraph' (mkGraphM ['a','b','c','d','e','f'] [('b','e',"be"), ('b','f',"bf")] >>= insEdge ('b','f', "bx") >>= insEdge ('b','e',"by"))) -- actual
               )
    ]

#if 0
reachableFrom :: TypeGraph -> TypeGraph
reachableFrom tg =
    set edges' edgeMap $ set (graph . _1) g' $ tg
    where
      edgeMap =
          Map.unionsWith Set.union
                 [ foldr (\n mp -> Map.insertWith Set.union n mempty mp) mempty (view roots tg :: Set A)
                 , foldr (\(_, n', _) mp -> Map.insertWith Set.union (tmap ! n') mempty mp) mempty (G.labEdges g')
                 , foldr (\(n, n', e) mp -> Map.insertWith Set.union (tmap ! n) (Set.singleton (e, tmap ! n')) mp) mempty (G.labEdges g') ]
      tmap :: Map G.Node A
      tmap = Map.fromList (G.labNodes g)
      g' :: G.Gr A B
      g' = reachableSubgraph (fmap (\t -> nmap ! t) (Set.toList (view roots tg))) g
      nmap :: Map A G.Node
      nmap = Map.fromList (fmap (\(n,t) -> (t,n)) (G.labNodes g))
      g :: G.Gr A B
      g = G.mkGraph lnodes ledges
      ledges = fromMaybe (error $ "reachableFrom - could not create edges:\n " ++
                          intercalate ",\n " (fmap (\t -> show (t, G.mkEdge nm t)) edgeTriples)) (G.mkEdges nm edgeTriples)
      (lnodes, nm) = G.mkNodes G.new (Map.keys (edgesFromGraph (view graph tg)))
      edgeTriples :: [(A, A, B)]
      edgeTriples = Set.toList $ unions $ fmap (\(t, hs) -> Set.map (\(h, t') -> (t, t', h)) hs) $ Map.toList $ edgesFromGraph $ view graph tg

-- |Return a subgraph of g which includes the nodes reachable from n
-- and the edges which connect any two of those nodes.
reachableSubgraph :: G.DynGraph gr => [G.Node] -> gr nl el -> gr nl el
reachableSubgraph ns g =
    G.mkGraph lnodes ledges
    where
      lnodes = fmap (\ (_, a, b, _) -> (a, b)) cs
      ledges = concatMap (\ (ins, a, _, outs) ->
                              catMaybes (fmap (\ (b, c) -> if p c then Just (c, a, b) else Nothing) ins ++
                                         fmap (\ (b, c) -> if p c then Just (a, c, b) else Nothing) outs)) cs
      p = (`Set.member` (Set.fromList nss))
      cs = fmap (G.context g) nss
      nss = concatMap (\n -> G.reachable n g) ns
#endif
