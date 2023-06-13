\documentclass[dvipdfmx,11pt]{article}
\usepackage{a4wide}
\usepackage{listings}
\usepackage{color}
\usepackage{bm}
\usepackage{amsmath}
\usepackage{amssymb}

\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
        \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}

\lstset
{
    language=Haskell,
    basicstyle=\small\ttfamily,
    flexiblecolumns=false,
    basewidth={0.5em,0.45em},
    frame=single,
    linewidth=40em,
    frameround=tttt, % makes four corners round
    keywordstyle={\color[rgb]{0,0,1}},
    stringstyle={\color[rgb]{0.6,0,0.6}},
    commentstyle={\color[rgb]{0,0.5,0}},
    framesep=10pt,
    breaklines=true,
    columns=fixed,
    numbers=none,
    xleftmargin=0zw,
    xrightmargin=0zw,
    numberstyle={\scriptsize},
    stepnumber=1,
    lineskip=0ex,
    tabsize=4,
    escapeinside={(@}{@)}
}
\newcommand{\fspace}{\mathcal{F}}
\newcommand{\lspace}{\mathcal{L}}

\begin{document}
\title{CART in Haskell}
\author{Genji Ohara}
\maketitle

\section{Preamble}
\begin{code}
import Text.ParserCombinators.Parsec
import Data.CSV
import Data.List

import DataSet
\end{code}

\section{Data Type Definition}
\subsection{Data Space}
\begin{align*}
    &\text{Feature Space}&\fspace&=\mathbb{R}^D \\
    &\text{Label Space}&\lspace&=\left\{0,1,\dots,L-1\right\} \\
    &\text{Data Space}&\mathcal{D}&=\fspace\times\lspace
\end{align*}
\begin{code}
type DataSet = [DataPoint]
\end{code}

\newpage
\subsection{Data Space}
\begin{code}
data Literal = Literal{lFeatureIdx :: Int, lValue :: Double} deriving Show

data Split = Split {sLiteral :: Literal, sScore :: Double} deriving Show

instance Eq Split where
    (Split l s) == (Split l' s') = s == s'

instance Ord Split where
    compare (Split l s) (Split l' s') = compare s s'

data Tree = Leaf {label :: Int} | 
            Node {literal :: Literal, left :: Tree, right :: Tree}
            deriving Show
\end{code}

\section{Gini Impurity}
\begin{align*}
    \mathrm{Gini}&:\lspace^n\to\mathbb{R} \\
    \mathrm{Gini}(L)&=1-\sum_{i=0}^{L-1}p_i(L)^2 \\
    p_i(L)&=\frac{1}{|L|}\sum_{l\in L}\mathbb{I}[l=i]
\end{align*}
\begin{code}
gini :: [Label] -> Double
gini labels = 1.0 - (sum $ map (^ 2) $ pList labels)

pList :: [Label] -> [Double]
pList labels = map (/ labelSetSize) $ map fromIntegral $ cntList labels 0
    where labelSetSize = fromIntegral $ length labels

cntList :: [Label] -> Int -> [Int]
cntList labels trg = 
    if trg == labelNum
        then []
        else [length $ filter (== trg) labels] ++ (cntList labels $ trg + 1)
\end{code}

\newpage
\section{Search Best Split}
\subsection{Split Data}
\begin{align*}
    D_l(D,i,v)&=\left\{(\bm{x},y)\in D\mid x_i<v\right\} \\
    D_r(D,i,v)&=\left\{(\bm{x},y)\in D\mid x_i\ge v\right\}
\end{align*}

\begin{code}
splitData :: DataSet -> Literal -> [DataSet]
splitData dataSet (Literal i v) = [lData, rData]
    where
        lData = [(DataPoint x y) | (DataPoint x y) <- dataSet, x !! i <= v]
        rData = [(DataPoint x y) | (DataPoint x y) <- dataSet, x !! i >  v]
\end{code}

\subsection{Score Splitted Data}
\begin{align*}
    \mathrm{score}(D,i,v)=\frac{|D_l|}{|D|}\mathrm{gini}\left[D_l(D,i,v)\right]
    +\frac{|D_r|}{|D|}\mathrm{gini}\left[D_r(D,i,v)\right]
\end{align*}

\begin{code}
scoreLiteral :: DataSet -> Literal -> Split
scoreLiteral dataSet literal = Split literal score
    where
        score = sum $ map (weightedGini (length dataSet)) $ labelSet
        labelSet = map (map dLabel) $ splitData dataSet literal 

weightedGini :: Int -> [Label] -> Double
weightedGini wholeSize labelSet = (gini labelSet) * dblDataSize / dblWholeSize
    where
        dblDataSize     = fromIntegral $ length labelSet
        dblWholeSize    = fromIntegral wholeSize
\end{code}

\subsection{Search Best Split}
\begin{align*}
    \underset{i,v}{\operatorname{argmin}}\operatorname{score}(D,i,v)
\end{align*}

\begin{code}
bestSplitAtFeature :: DataSet -> Int -> Split
bestSplitAtFeature dataSet i = myMin splitList
    where
        splitList   = [scoreLiteral dataSet l | l <- literalList]
        literalList = [Literal i (x !! i) | (DataPoint x y) <- dataSet]

bestSplit :: DataSet -> Split
bestSplit dataSet = myMin splitList
    where splitList = [bestSplitAtFeature dataSet f | f <- [0,1..featureNum-1]]
\end{code}

\begin{code}
growTree :: DataSet -> Int -> Int -> Tree
growTree dataSet depth maxDepth =
    if depth == maxDepth
        then Leaf $ majorLabel [y | (DataPoint x y) <- dataSet]
        else Node literal leftTree rightTree
    where
        literal         = sLiteral $ bestSplit dataSet
        leftTree        = growTree lData (depth + 1) maxDepth
        rightTree       = growTree rData (depth + 1) maxDepth
        [lData, rData]  = splitData dataSet literal

majorLabel :: [Label] -> Label
majorLabel labels = maxIndex $ cntList labels 0
\end{code}


\section{Main}
\begin{code}
branchToString :: Int -> String
branchToString depth = "|" ++ (concat $ replicate depth "   |") ++ "--- "

treeToString :: Tree -> Int -> String
treeToString (Leaf label) depth = 
    branchToString depth ++ "class: " ++ (show label) ++ "\n"
treeToString (Node (Literal i v) leftTree rightTree) depth =
    let
        str1 = branchToString depth ++ "Feature[" ++ (show i) ++ "] "
        str2 = "< " ++ (show v) ++ "\n"
        str3 = treeToString leftTree $ depth + 1
        str4 = ">= " ++ (show v) ++ "\n"
        str5 = treeToString rightTree $ depth + 1
    in str1 ++ str2 ++ str3 ++ str1 ++ str4 ++ str5
    


main = do
    rawDataSet <- parseFromFile csvFile "../data/iris/iris.data"
    let dataSet = either (\x -> []) processData rawDataSet
    print $ bestSplit dataSet
    print $ scoreLiteral dataSet $ Literal 2 2.45
    putStrLn $ treeToString (growTree dataSet 0 4) 0



myMin :: [Split] -> Split
myMin splitList = foldr min (Split (Literal 0 0) 2) splitList

myMax :: [Split] -> Split
myMax splitList = foldr max (Split (Literal 0 0) (-1)) splitList

maxIndex :: Ord a => [a] -> Int
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

\end{code}

\end{document}
