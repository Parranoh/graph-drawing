module Graph where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Bifunctor as BF
import Control.Monad

compose :: [a -> a] -> a -> a
compose = foldr (.) id

split :: Eq a => a -> [a] -> [[a]]
split sep = go
    where
        go [] = []
        go xs = let (y,xs') = span (/= sep) xs
                in y : go (drop 1 xs')

type Vertex = Int
type Edge = (Vertex,Vertex)
type Embedding = Vertex -> [Edge]

data Graph = Graph (S.Set Vertex) (M.Map Vertex (S.Set Vertex,S.Set Vertex)) deriving (Show)

vertices :: Graph -> [Vertex]
vertices (Graph v _) = S.toList v

edges :: Graph -> [Edge]
edges (Graph _ m) = [(v,w) | (v,ws) <- map (BF.second (S.toList . fst)) . M.toList $ m, w <- ws]

directedGraph :: [Vertex] -> [Edge] -> Graph
directedGraph vs es = Graph (S.fromList vs) $ L.foldl' (flip f) mInit es
    where
        mInit = M.fromList [(v,(S.empty,S.empty)) | v <- vs]

        f :: Edge -> M.Map Vertex (S.Set Vertex,S.Set Vertex) -> M.Map Vertex (S.Set Vertex,S.Set Vertex)
        f e@(v,w) = M.update (Just . (BF.first  . S.insert $ w)) v .
                    M.update (Just . (BF.second . S.insert $ v)) w

toLatex :: (Vertex -> (Int,Int)) -> Graph -> ShowS
toLatex pos g =
    showString "\\begin{tikzpicture}\n\\tikzset{>=Latex}\n" .
    compose
        [ showString "\\node[circle,draw,fill=white] ("
        . shows v
        . showString ") at "
        . shows (pos v)
        . showString "{};\n"
    | v <- vertices g] .
    compose
        [ showString "\\draw[->] ("
        . shows v
        . showString ") to ("
        . shows w
        . showString ");\n"
    | (v,w) <- edges g] .
    showString "\\end{tikzpicture}\n"

defaultPos :: Vertex -> (Int,Int)
defaultPos = flip (,) 0

insertVertex :: Vertex -> Graph -> Graph
insertVertex v (Graph vs es) = Graph (S.insert v vs) es

insertNewVertex :: Graph -> (Vertex,Graph)
insertNewVertex (Graph vs es) = (v,Graph (S.insert v vs) es)
    where
        v = f $ S.size vs
        f v | v `S.notMember` vs = v
            | otherwise = f $ v + 1

insertVertexMaybe :: Vertex -> Graph -> Maybe Graph
insertVertexMaybe v (Graph vs es)
    | v `S.notMember` vs = Just $ Graph (S.insert v vs) es
    | otherwise = Nothing

-- dfs :: Graph -> Vertex -> [Vertex]
