{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2021
-}
module Assign_3 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: TODO add name
-- Date: TODO add date
macid :: String
macid = "habinskw"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
data Graph a = Graph { getNodes :: [Node a]
                     , getEdges :: [Edge] }
  deriving (Show,Eq)

type Edge = (NodeID,NodeID)
type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq,Ord)


{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC,nodeD :: Node Char
nodeA = Node 0 'a'
nodeB = Node 1 'b'
nodeC = Node 2 'c'

nodeD = Node 3 'K'

exGraph :: Graph Char
exGraph =
  let
    nodes = [nodeA,nodeB,nodeC]
    edges = [(1,1),(1,2),(3,2)]
  in Graph nodes edges

{- -----------------------------------------------------------------
 - maxNodeID
 - -----------------------------------------------------------------
 - Description:
 -    Function takes in a type Graph a as input and returns the largest NodeID in the graph as output of type Maybe NodeID

 -    The function recursively iterates through every NodeID in the graph by pattern matching. If the current NodeID is greater
 -    or equal tho the following NodeID, then it recurses through the rest of the Graph by returning the Graph without that smallest
 -    node. Otherwise it will recurse through by returning the Graph without the current Node.

 -    The base cases include when there is only 1 Node left in which case it will return that NodeID. As well as when there is no
 -    nodes in the graph in which case it will return Nothing
 -}

maxNodeID :: Graph a -> Maybe NodeID
maxNodeID (Graph [] [])     = Nothing
maxNodeID (Graph [x] ys)    = Just(getNodeID x)
maxNodeID (Graph (x:y:xs) ys)
 | getNodeID x >= getNodeID y   = maxNodeID(Graph (x:xs) ys)
 | otherwise                    = maxNodeID(Graph (y:xs) ys)

{- -----------------------------------------------------------------
 - insertNode
 - -----------------------------------------------------------------
 - Description:
 -    The function takes in a character and a Graph as input and returns a Graph as output.

 -    The function takes a Graph and adds a node with NodeID 1 larger than previous largest and 
 -    NodeVal as the character that was inputed. This is done by pattern matching the list of nodes 
 -    in the graph and concattinating that list with a new Node. 
 
 -    The two cases consist of when the graph contains no nodes in which case it will add a node of 
 -    NodeID 0 and NodeVal v. 
 -    The other case is when the graph contains other nodes in which case it calls maxNodeID to find the
 -    previous highest NodeID and adds 1 to it. This is done using an auxillary function which converts
 -    the maxNodeID of type Maybe NodeID to type Int
 -}



insertNode :: a -> Graph a -> Graph a
insertNode v (Graph [] [])         = Graph [Node 0 v] []
insertNode v (Graph nodes edges)   = Graph (nodes ++ [Node (toInt (maxNodeID (Graph nodes edges)) + 1) v]) edges
  where
    toInt :: Maybe NodeID -> Int
    toInt (Just n) = n


{- -----------------------------------------------------------------
 - removeNode
 - -----------------------------------------------------------------
 - Description:
 -    The function takes in a NodeID and a Graph as input and returns an altered Graph as output. The purpose
 -    of the function is to return a Graph without any nodes that habe the same node Id as the given value

 -    removeNode calls on 2 auxillary functions

 -    nodeList is a function that takes a node Id and a list of nodes and returns an altered list as output
 -    It's purpose is to recursively itereate through the list of nodes and return only the nodes whose NodeID 
 -    doesn't match the given input

 -    edgesList is a function that takes a node Id and a list of edges and returns an altered list of edges as
 -    output. It's purpose is to recursively iterate through the list and only return the edges that do not contain
 -    the nodeId as part of components

 -    The removeNode function combines nodeList and edgeList to return a graph that consists of a list of nodes and
 -    a list of edges that do not contain the inputed nodeID
 -}

removeNode :: NodeID -> Graph a -> Graph a
removeNode nID (Graph nodes edges) = Graph (nodeList nID nodes) (edgeList nID edges)
  where 
    nodeList :: NodeID -> [Node a] -> [Node a]
    nodeList _ [] = []
    nodeList nID (n:ns)
      | nID == getNodeID n  = nodeList nID ns
      | otherwise           = n : nodeList nID ns
    edgeList :: NodeID -> [Edge] -> [Edge]
    edgeList _ [] = []
    edgeList nID (e:es)
     | nID == fst e     = edgeList nID es
     | nID == snd e     = edgeList nID es
     | otherwise        = e : edgeList nID es

{- -----------------------------------------------------------------
 - lookupNode
 - -----------------------------------------------------------------
 - Description:
 -    The function takes a Node ID and a Graph as input and returns a Maybe Node as output. 

 -    The purpose of the function is to return a node that corresponds to the give node ID value
 -    If such a node does not exists then nothing is returned

 -    The function uses pattern matching to iterate through the list of nodes. It checks to see
 -    if the given value is equal to the NodeID. If true, then return the node. If false, recursively
 -    checks the next case until the base case is reached.
 -}

lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID (Graph [] _ ) = Nothing
lookupNode nID (Graph (n:ns) edges)
  | nID == getNodeID n  = Just $ Node (getNodeID n) (getNodeVal n)
  | otherwise           = lookupNode nID (Graph ns edges)

{- -----------------------------------------------------------------
 - insertEdge
 - -----------------------------------------------------------------
 - Description:
 -   The fucntion takes is an Edge of type (NodeID,NodeID) and a Graph as inputs and returns a maybe Graph as output
 -   
 -   The purpose of the function is to insert a given Edge into a graph. If the edge already exists, just return the 
 -   graph unaltered. If the Nodes that correspond to the edges do not exist, then return nothing.

 -   The function uses two auxillary functions 
 -   containsBothNodes returns true if both nodes corresponding to the edge is in the graph and false if one or more is not
 -   containsEdge returns true if the edge already exists in the graph and returns false if it does not
 -}

insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge _       (Graph [] _)  = Nothing
insertEdge (n1,n2) (Graph ns es)
  | not containsBothNodes       = Nothing
  | containsEdge (Graph ns es)  = Just $ Graph ns es
  | otherwise                   = Just $ Graph ns (es ++ [(n1,n2)])
  where
    containsBothNodes :: Bool
    containsBothNodes
     | lookupNode n1 (Graph ns es) == Nothing = False
     | lookupNode n2 (Graph ns es) == Nothing = False
     | otherwise                              = True
    containsEdge :: Graph a -> Bool
    containsEdge (Graph _ []) = False
    containsEdge (Graph ns (e:es))
     | (n1,n2) == e = True
     | otherwise = containsEdge (Graph ns es)



{-

TEST CASES

--------------------------------------
Graph A1 [Node 12 "idc", Node 4378 "cdi", Node 67 "icd"] []
Graph A2 [Node 1 ""] []
Graph A3 [][]

Function: maxNodeID
Test Case Number: 1
Input: Graph A1
Expected Output: Just 4378
Actual Output: Just 4378

Function: maxNodeID
Test Case Number: 2
Input: Graph A2
Expected Output: Just 1
Actual Output: just 1

Function: maxNodeID
Test Case Number: 3
Input: Graph A3
Expected Output: Nothing
Actual Output: Nothing
--------------------------------------

--------------------------------------
Graph B = [Node 17 "x", Node 115 "y", Node 1 "z"] [(115,115), (17,1), (1,115)]

Function: insertNode
Test Case Number: 1
Input: "potato" Graph B
Expected Output: Graph [Node 17 "x", Node 115 "y", Node 1 "z", Node 116 "potato"] [(115,115), (17,1), (1,115)]
Actual Output: Graph [Node 17 "x", Node 115 "y", Node 1 "z", Node 116 "potato"] [(115,115), (17,1), (1,115)]

Function: insertNode
Test Case Number: 2
Input: "hello" Graph [] []
Expected Output: Graph [Node 0 "hello"] []
Actual Output: Graph [Node 0 "hello"] []

Function: insertNode
Test Case Number: 3
Input: "3 test cases is too much" Graph B
Expected Output: [Node 17 "x", Node 115 "y", Node 1 "z", Node 116 "3 test cases is too much"] [(115,115), (17,1), (1,115)]
Actual Output:[Node 17 "x", Node 115 "y", Node 1 "z", Node 116 "3 test cases is too much"] [(115,115), (17,1), (1,115)]
--------------------------------------

--------------------------------------
Graph C = [Node 2 "U", Node 4 "K", Node 1 "F", Node 3 "C"] [(2,4) (1,3) (2,2)]

Function: removeNode
Test Case Number: 1
Input: 3 Graph C
Expected Output: Graph [Node 2 "U", Node 4 "K", Node 1 "F"] [(2,4),(2,2)]
Actual Output: Graph [Node 2 "U", Node 4 "K", Node 1 "F"] [(2,4),(2,2)]

Function: removeNode
Test Case Number: 2
Input: 9099109109 Graph C
Expected Output: Graph C
Actual Output: Graph C

Function: removeNode
Test Case Number: 3
Input: 2 Graph C
Expected Output: Graph [Node 4 "K", Node 1 "F", Node 3 "C"] [(1,3)]
Actual Output: Graph [Node 4 "K", Node 1 "F", Node 3 "C"] [(1,3)]
--------------------------------------

--------------------------------------

Graph D = [Node 3 "I", Node 1 "T", Node 4 "S", Node 2 "H"] [(1,1)]

Function: lookupNode
Test Case Number: 1
Input: 3 Graph D
Expected Output: Node 3 "I"
Actual Output: Just Node 3 "I"

Function: lookupNode
Test Case Number: 2
Input: 99 Graph D
Expected Output: Nothing
Actual Output: Nothing

Function: lookupNode
Test Case Number: 3
Input: (1,1)
Expected Output: Error
Actual Output: Error - Couldn't match type (Integer,Integer) with Int
--------------------------------------

--------------------------------------

Graph E = [Node 0 "a", Node 1 "b", Node 3 "c"] [(1,1), (0,1) (0,3), (1,3)]

Function: insertEdge
Test Case Number: 1
Input: (1,1) Graph E
Expected Output: Graph E
Actual Output: Graph E

Function: insertEdge
Test Case Number: 2
Input: (3,3)
Expected Output: Graph [Node 0 "a", Node 1 "b", Node 3 "c"] [(1,1), (0,1) (0,3), (1,3), (3,3)]
Actual Output: Graph [Node 0 "a", Node 1 "b", Node 3 "c"] [(1,1), (0,1) (0,3), (1,3), (3,3)]

Function: insertEdge
Test Case Number: 3
Input: (19,9)
Expected Output: Nothing
Actual Output: Nothing
--------------------------------------



-}

