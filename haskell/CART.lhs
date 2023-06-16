\documentclass[dvipdfmx,11pt]{article}
\usepackage{a4wide}
\usepackage{listings}
\usepackage{color}
\usepackage{bm}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage[dvipdfmx]{graphicx}

\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
        \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}

\lstset
{
    % language=Haskell,
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
import Numeric.LinearAlgebra
import Prelude hiding ((<>))

import Text.ParserCombinators.Parsec
import Data.CSV
import Data.List

type Vec = Vector R
type Mat = Matrix R
\end{code}

\section{Data Type Definition}
\subsection{Data Space}
\begin{align*}
    &\text{Feature Space}&\fspace&=\mathbb{R}^D \\
    &\text{Label Space}&\lspace&=\left\{0,1,\dots,L-1\right\} \\
    &\text{Data Space}&\mathcal{D}&=\fspace\times\lspace
\end{align*}
\begin{code}
type Feature    = [Double]
type Label      = Int
type DataSet    = [DataPoint]

data DataPoint = DataPoint {
    dFeature :: Feature,
    dLabel   :: Label
} deriving Show
\end{code}

\newpage
\subsection{Constants}
\begin{code}
featureNum :: Int
featureNum = 4

labelNum :: Int
labelNum = 3
\end{code}

\subsection{Tree Structure}
\subsubsection{Literal}
\begin{code}
data Literal = Literal {
    lFeatureIdx :: Int,
    lValue :: Double
} deriving Show
\end{code}

\subsubsection{Split}
\begin{code}
data Split = Split {
    sLiteral :: Literal,
    sScore :: Double
} deriving Show

instance Eq Split where
    (Split l s) == (Split l' s') = s == s'

instance Ord Split where
    compare (Split l s) (Split l' s') = compare s s'
\end{code}

\subsubsection{Tree}
\begin{code}
data Tree = Leaf {label :: Int, id :: String} | 
            Node {literal :: Literal, left :: Tree, right :: Tree, id :: String}
            deriving Show
\end{code}

\newpage
\section{Gini Impurity}
\subsection{Class Ratio}
\begin{align*}
    &\text{Label Set}&&L=\left\{y\mid(\bm{x},y)\in D\right\} \\[7pt]
    &\text{Label Count}&&c_l(L)=\sum_{i\in L}\mathbb{I}[i=l],\quad
        &&\bm{c}(L)=\sum_{i\in L}\operatorname{onehot}(i) \\[4pt]
    &\text{Class Ratio}&&p_l(L)=\frac{c_l(L)}{|L|},\quad
        &&\bm{p}(L)=\frac{\bm{c}(L)}{\|\bm{c}(L)\|_1}
\end{align*}
\begin{code}
labelCount :: [Label] -> Vec
labelCount = sum . (map $ oneHotVector labelNum)

classRatio :: [Label] -> Vec
classRatio labels = scale (1 / (norm_1 countVec)) $ countVec
    where countVec = labelCount labels
\end{code}

\subsection{Gini Impurity}
\begin{align*}
    % \mathrm{Gini}&:\lspace^n\to\mathbb{R} \\
    \mathrm{Gini}(L)&=1-\sum_{l=0}^{L-1}p_l(L)^2=1-\|\bm{p}(L)\|_2^2
\end{align*}
\begin{code}
gini :: [Label] -> Double
gini labels = 1.0 - (norm_2 $ classRatio labels) ^ 2
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

\section{Grow Tree}
\subsection{Grow Tree}
\begin{code}
growTree :: DataSet -> Int -> Int -> String -> Tree
growTree dataSet depth maxDepth id =
    if stopGrowing
        then Leaf (majorLabel dataSet) id
        else Node literal leftTree rightTree id
    where
        literal         = sLiteral $ bestSplit dataSet
        leftTree        = growTree lData (depth + 1) maxDepth (id ++ "l")
        rightTree       = growTree rData (depth + 1) maxDepth (id ++ "r")
        [lData, rData]  = splitData dataSet literal
        stopGrowing =
            depth == maxDepth || 
            gini [y | (DataPoint x y) <- dataSet] == 0 ||
            length lData == 0 || length rData == 0
\end{code}

\subsection{Stop Growing}
$$
    \operatorname{majorLabel}(D)=\underset{l\in\lspace}
    {\operatorname{argmax}}\sum_{(\bm{x},y)\in D}\mathbb{I}\left[y=l\right]
$$
\begin{code}
majorLabel :: DataSet -> Label
majorLabel dataSet = maxIndex $ labelCount [y | (DataPoint x y) <- dataSet]
\end{code}

\newpage
\section{Output Tree}
\begin{code}
literalToStr :: Literal -> Bool -> String
literalToStr (Literal i v) less = 
    "Feature[" ++ (show i) ++ "]" ++ if less then "< " else ">= " ++ (show v)

branchToString :: Int -> String
branchToString depth = "|" ++ (concat $ replicate depth "   |") ++ "--- "

treeToString :: Tree -> Int -> String
treeToString (Leaf label id) depth = 
    branchToString depth ++ "class: " ++ (show label) ++ "\n"
treeToString (Node (Literal i v) leftTree rightTree id) depth =
    let
        str1 = branchToString depth ++ "Feature[" ++ (show i) ++ "] "
        str2 = "< " ++ (show v) ++ "\n"
        str3 = treeToString leftTree $ depth + 1
        str4 = ">= " ++ (show v) ++ "\n"
        str5 = treeToString rightTree $ depth + 1
    in str1 ++ str2 ++ str3 ++ str1 ++ str4 ++ str5
\end{code}
\lstinputlisting[caption=Example of CLI output]{output-tree}

\newpage
\section{Output Tree in GraphViz}
\begin{code}
labelToStringForGraphViz :: Tree -> String
labelToStringForGraphViz (Leaf label id) =
    id ++ " [label=\"Class: " ++ (show label) ++ "\"]\n"
labelToStringForGraphViz (Node (Literal i v) left right id) =
    id ++ " [shape=box,label=\"Feature[" ++ (show i) ++ "] < " ++ (show v) ++ "\"]\n" ++
    labelToStringForGraphViz left ++ labelToStringForGraphViz right

nodeToStringForGraphViz :: Tree -> String
nodeToStringForGraphViz (Leaf label id) = id ++ ";\n"
nodeToStringForGraphViz (Node lLiteral left right id) =
    id ++ " -- " ++ nodeToStringForGraphViz left ++
    id ++ " -- " ++ nodeToStringForGraphViz right

treeToStringForGraphViz :: Tree -> String
treeToStringForGraphViz tree = 
    "graph Tree {\n" ++ labelToStringForGraphViz tree ++ nodeToStringForGraphViz tree ++ "}"
\end{code}
\begin{figure}[htbp]
    \centering
    \includegraphics[width=15cm]{output-tree.png}
    \caption{Example of GraphViz output}
\end{figure}


\newpage
\section{Main}
\begin{code}
main = do
    rawDataSet <- parseFromFile csvFile "../data/iris/iris.data"
    let dataSet = either (\x -> []) processData rawDataSet
    let tree = growTree dataSet 0 10 "n"
    putStrLn $ treeToString tree 0
    writeFile "tree.dot" $ treeToStringForGraphViz tree
\end{code}

\section{Other Functions}
\subsection{I-O \& Data Processing}
\begin{code}
strLabelToIntLabel :: String -> Int
strLabelToIntLabel str
    | str == "Iris-setosa"      = 0
    | str == "Iris-versicolor"  = 1
    | str == "Iris-virginica"   = 2
    | otherwise                 = 3

processDataPoint :: [String] -> DataPoint
processDataPoint strs = DataPoint feature label
    where
        feature = map (read :: String -> Double) $ init strs
        label   = strLabelToIntLabel $ last strs

processData :: [[String]] -> [DataPoint]
processData rawData = map processDataPoint rawData
\end{code}

\subsection{Algorithm}
\begin{code}
myMin :: [Split] -> Split
myMin splitList = foldr min (Split (Literal 0 0) 2) splitList

myMax :: [Split] -> Split
myMax splitList = foldr max (Split (Literal 0 0) (-1)) splitList

myMaxIndex :: Ord a => [a] -> Int
myMaxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

oneHotList :: Int -> Int -> [Double]
oneHotList len idx =
    if len == 0
        then []
        else
            if idx == 0
                then 1 : oneHotList (len - 1) (idx - 1)
                else 0 : oneHotList (len - 1) (idx - 1)

oneHotVector :: Int -> Int -> Vec
oneHotVector len idx = vector $ oneHotList len idx
\end{code}

\end{document}
