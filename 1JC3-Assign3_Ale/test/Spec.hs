{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : 1JC3-Assign3.Spec
Copyright   :  (c) Curtis D'Alves 2020
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Contains Quickcheck tests for Exercises01 and a main function that runs each tests and prints the results
-}
module Main where

import qualified Assign_3 as A3
import Assign_3 (Graph (..),Edge,Node (..))
import Data.List (nub,find,delete,intercalate,(\\))
import Data.Char
import Data.Fixed (mod')
import Test.QuickCheck (quickCheck
                       ,quickCheckResult
                       ,quickCheckWithResult
                       ,stdArgs
                       ,maxSuccess
                       ,Result(Success)
                       ,within
                       ,Testable
                       ,Positive (..)
                       ,getPositive
                       ,Arbitrary
                       ,arbitrary)
import Test.Hspec
import Test.QuickCheck.Property (property)

{- -----------------------------------------------------------------
 -  QuickCheck Properties
 - -----------------------------------------------------------------
 -}
-- | Existential type wrapper for QuickCheck propositions, allows @propList@ to essentially
--   act as a heterogeneous list that can hold any quickcheck propositions of any type
data QuickProp = forall prop . Testable prop =>
                 QuickProp { quickPropName :: String
                           , quickPropMark :: Int
                           , quickPropFunc :: prop
                           }

-- | The maximum number of NodeID's in a generated Graph
graphSize = 5

instance Arbitrary a => Arbitrary (Graph a) where
  arbitrary = do xs <- arbitrary
                 let edges = nub $ map (\(n,m) -> (getPositive n `mod` graphSize,
                                                   getPositive m `mod` graphSize)) xs
                 nodes <- mapM (\n -> do v <- arbitrary
                                         return (Node n v)) $ nub
                          $ concatMap (\(n,m) -> [n,m]) edges

                 return $ Graph nodes edges

newtype ArbitraryNodeID = ArbitraryNodeID Int
  deriving (Show,Eq,Ord)
instance Arbitrary ArbitraryNodeID where
  arbitrary = do (Positive x) <- arbitrary
                 return (ArbitraryNodeID (x `mod` graphSize))

{- -----------------------------------------------------------------
 - maxNodeIDProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if maxNodeID returns the maximum
 -    NodeID fro a given graph. Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck maxNodeIDProp
 - -----------------------------------------------------------------
 - |  Input       | Graph (ns,es) :: Graph Char                    |
 - -----------------------------------------------------------------
 - |  Output      |                                                |
 - |   ns = []    | True if output = Nothing                       |
 - |   otherwise  | forall n in ns                                 |
 - |              |      maxNodeID graph >= getNodeID n            |
 - -----------------------------------------------------------------
 -}
maxNodeIDProp :: Graph Char -> Bool
maxNodeIDProp graph@(Graph ns es) =
  let
    maybeMaxID          = A3.maxNodeID graph
  in case maybeMaxID of
       Just maxID -> and [ maxID >= getNodeID n | n <- ns ]
       Nothing    -> null ns

{- -----------------------------------------------------------------
 - insertNodeProp
 - -----------------------------------------------------------------
 - Description:
 -    Quickcheck property, tests if insertNode returns a valid graph
 -    with the given Node properly inserted.
 -    Call this using Test.QuickCheck, i.e
 -         > import Test.QuickCheck (quickCheck)
 -         > quickCheck insertNodeProp
 - -----------------------------------------------------------------
 - |  Input              | Graph (ns,es) :: Graph Char             |
 - -----------------------------------------------------------------
 - |  Output             |                                         |
 - |                     |  True iff                               |
 - |                     |   when graph' = insertNode 'a' graph    |
 - |                     |    graph' // graph == [Node i v]        |
 - |                     |    and isMaxID i and v == 'a'           |
 - -----------------------------------------------------------------
 -}
insertNodeProp0 :: Graph Char -> Bool
insertNodeProp0 graph@(Graph nodes edges) =
  let
    graph' = A3.insertNode 'a' graph
    -- use this to test that a NodeID is the maximumal NodeID in a graph
    isMaxID n = and [ getNodeID n >= getNodeID n' | n' <- getNodes graph' ]
    -- a node with the value 'a' and the maximal NodeID should exist
    newNodeIsMaximal = or [ isMaxID n | n <- filter (\(Node i v) -> v == 'a')  (getNodes graph') ]
    -- graph' only has one extra element, the node with the maximal NodeID
    nodesDiff = case ((getNodes graph') \\ (getNodes graph)) of
                  [node] -> (getNodeVal node == 'a') && (isMaxID node)
                  _ -> False
    -- graph' has the same edges
    edgesDiff = edges == getEdges graph'
  in newNodeIsMaximal

insertNodeProp1 :: Graph Char -> Bool
insertNodeProp1 graph@(Graph nodes edges) =
  let
    graph' = A3.insertNode 'a' graph
    -- use this to test that a NodeID is the maximumal NodeID in a graph
    isMaxID n = and [ getNodeID n >= getNodeID n' | n' <- getNodes graph' ]
    -- a node with the value 'a' and the maximal NodeID should exist
    newNodeIsMaximal = or [ isMaxID n | n <- filter (\(Node i v) -> v == 'a')  (getNodes graph') ]
    -- graph' only has one extra element, the node with the maximal NodeID
    nodesDiff = case ((getNodes graph') \\ (getNodes graph)) of
                  [node] -> (getNodeVal node == 'a') && (isMaxID node)
                  _ -> False
    -- graph' has the same edges
    edgesDiff = edges == getEdges graph'
  in nodesDiff && edgesDiff

-- {- -----------------------------------------------------------------
--  - removeNodeProp
--  - -----------------------------------------------------------------
--  - Description:
--  -    Quickcheck property, tests if removeNode returns a valid graph
--  -    with the given Node properly removed.
--  -    Call this using Test.QuickCheck, i.e
--  -         > import Test.QuickCheck (quickCheck)
--  -         > quickCheck removeNodeProp
--  - -----------------------------------------------------------------
--  - |  Input              | 0 <= nID < graphSize :: Int             |
--  - |                     | graph :: Graph Char                     |
--  - -----------------------------------------------------------------
--  - |   Output            |                                         |
--  - |                     |  True iff                               |
--  - |                     |  when graph' = removeNode nID graph     |
--  - |                     |   nID is not in graph' (nodes or edges) |
--  - |                     |   all other nodes in graph' are in graph|
--  - |                     |   all edges not containing nID in graph |
--  - |                     |   are in graph'                         |
--  - -----------------------------------------------------------------
--  -}
removeNodeProp0 :: ArbitraryNodeID ->  Graph Char -> Bool
removeNodeProp0 (ArbitraryNodeID nID) graph =
  let
    -- use removeNode to remove the Node with nID
    graph'  = A3.removeNode nID graph
    -- getNodes graph' should not contain nID
    removedFromNodes = not (nID `elem` (map getNodeID  $ getNodes graph'))
    -- getEdges graph' should not contain nID
    removedFromEdges = and [ nID /= x && nID /= y | (x,y) <- getEdges graph' ]
    -- graph' only has one extra element, the node with the maximal NodeID
    nodesDiff = case ((getNodes graph) \\ (getNodes graph')) of
                  [node] -> getNodeID node == nID
                  _      -> not (nID `elem` (map getNodeID $ getNodes graph))
    -- graph' only removes edges that contain nID
    edgesDiff = case ((getEdges graph) \\ (getEdges graph')) of
                  es -> and [ e0 == nID || e1 == nID | (e0,e1) <- es ]
  in removedFromNodes
  && removedFromEdges

removeNodeProp1 :: ArbitraryNodeID ->  Graph Char -> Bool
removeNodeProp1 (ArbitraryNodeID nID) graph =
  let
    -- use removeNode to remove the Node with nID
    graph'  = A3.removeNode nID graph
    -- getNodes graph' should not contain nID
    removedFromNodes = not (nID `elem` (map getNodeID  $ getNodes graph'))
    -- getEdges graph' should not contain nID
    removedFromEdges = and [ nID /= x && nID /= y | (x,y) <- getEdges graph' ]
    -- graph' only has one extra element, the node with the maximal NodeID
    nodesDiff = case ((getNodes graph) \\ (getNodes graph')) of
                  [node] -> getNodeID node == nID
                  _      -> not (nID `elem` (map getNodeID $ getNodes graph))
    -- graph' only removes edges that contain nID
    edgesDiff = case ((getEdges graph) \\ (getEdges graph')) of
                  es -> and [ e0 == nID || e1 == nID | (e0,e1) <- es ]
  in nodesDiff
  && edgesDiff

-- {- -----------------------------------------------------------------
--  - lookupNodeProp
--  - -----------------------------------------------------------------
--  - Description:
--  -    Quickcheck property, tests if lookupNode returns the proper
--  -    Node for a given NodeID.
--  -    Call this using Test.QuickCheck, i.e
--  -         > import Test.QuickCheck (quickCheck)
--  -         > quickCheck lookupNodeProp
--  - -----------------------------------------------------------------
--  - |  Input              | i :: Int                                |
--  - |                     | edges :: [(Int,Int)]                    |
--  - -----------------------------------------------------------------
--  - |   Output            |                                         |
--  - |                     |  True iff case lookupNode nID graph     |
--  - |                     |   Just n  | n == nID and n is in graph  |
--  - |                     |   Nothing | nID is not in graph         |
--  - -----------------------------------------------------------------
--  -}
lookupNodeProp :: ArbitraryNodeID -> Graph Char -> Bool
lookupNodeProp (ArbitraryNodeID nID) graph =
  case A3.lookupNode nID graph of
     Just node -> (getNodeID node == nID) && node `elem` (getNodes graph)
     Nothing   -> not (nID `elem` (map getNodeID $ getNodes graph))


-- {- -----------------------------------------------------------------
--  - insertEdgeProp
--  - -----------------------------------------------------------------
--  - Description:
--  -    Quickcheck property, tests if insertEdge returns a valid graph
--  -    with the given Node properly inserted.
--  -    Call this using Test.QuickCheck, i.e
--  -         > import Test.QuickCheck (quickCheck)
--  -         > quickCheck insertEdgeProp
--  - -----------------------------------------------------------------
--  - |  Input              | (n0,n1) :: (NodeID,NodeID)              |
--  - |                     | graph   :: Graph Char                   |
--  - -----------------------------------------------------------------
--  - |   Output            |                                         |
--  - |                     |  True iff case insertEdge (n0,n1) graph |
--  - |                     |    Just (Graph ns es)                   |
--  - |                     |     | all ns are in graph               |
--  - |                     |     | es \\ getEdges graph == [(n0,n1)] |
--  - |                     |     | es has no duplicates              |
--  - |                     |    Nothing                              |
--  - |                     |     | neither n0 or n1 are in graph     |
--  - -----------------------------------------------------------------
--  -}
insertEdgeProp :: (ArbitraryNodeID,ArbitraryNodeID) -> Graph Char -> Bool
insertEdgeProp (ArbitraryNodeID n0,ArbitraryNodeID n1) graph =
  let
    graph'       = A3.insertEdge (n0,n1) graph
  in  case graph' of
        Just (Graph ns es) -> ns == getNodes graph
                           && (n0 `elem` (map getNodeID $ getNodes graph ))
                           && (n1 `elem` (map getNodeID $ getNodes graph ))
                           && (es \\ getEdges graph == [(n0,n1)] || (n0,n1) `elem` getEdges graph)
                           && (length es - length (getEdges graph) == 1  || es == getEdges graph)
        Nothing -> not (n0 `elem` (map getNodeID $ getNodes graph))
                   || not (n1 `elem` (map getNodeID $ getNodes graph))

-------------------------------------------------------------------------------------------
-- * Run Tests
main :: IO ()
main = hspec $ do
  describe "maxNodeID graph" $ do
    it "returns a NodeID larger than any other NodeID in its input " $ property $ maxNodeIDProp
  describe "insertNode n graph" $ do
    it "newly inserted node is maximal" $ property $ insertNodeProp0
    it "the difference between the graphs is only a single Node entry" $ property $ insertNodeProp1
  describe "removeNode nID graph" $ do
    it "nID should not appear in any of the new graphs nodes or edges" $ property $ removeNodeProp0
    it "the rest of the nodes/edges should remain the same" $ property $ removeNodeProp1
  describe "lookupNode nID graph" $ do
    it "should return a node with NodeID nID or nothing if nID isn't in graph" $ property $ lookupNodeProp
  describe "insertEdge (n0,n1) graph" $ do
    it "should insert an edge iff n0 and n1 are in graph, nothing otherwise" $ property $ insertEdgeProp
