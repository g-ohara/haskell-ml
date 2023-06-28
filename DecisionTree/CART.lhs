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
import DataProcessing
import Numeric.LinearAlgebra
import Prelude hiding ((<>))

type Vec = Vector R
-- type Mat = Matrix R
\end{code}

\section{Data Type Definition}
\subsection{Data Space}
\begin{align*}
    &\text{Feature Space}&\fspace&=\mathbb{R}^D \\
    &\text{Label Space}&\lspace&=\left\{0,1,\dots,L-1\right\} \\
    &\text{Data Space}&\mathcal{D}&=\fspace\times\lspace
\end{align*}

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
data Literal = Literal Int Double
-- data Literal = Literal {
--     lFeatureIdx :: Int,
--     lValue :: Double
-- } 

instance Show Literal where
    show (Literal i v) = "Feature[" ++ (show i) ++ "] < " ++ (show v)
\end{code}

\subsubsection{Split}
\begin{code}
data Split = Split {
    sLiteral :: Literal,
    sScore :: Double
} deriving Show

instance Eq Split where
    (Split _ s) == (Split _ s') = s == s'

instance Ord Split where
    compare (Split _ s) (Split _ s') = compare s s'
\end{code}

\subsubsection{Tree}
\begin{code}
data Tree = Leaf Int String | Node Literal Tree Tree String
-- data Tree = Leaf {label :: Int, id :: String} | 
--             Node {literal :: Literal, left :: Tree, right :: Tree, id :: String}
\end{code}

\newpage
\section{Output Tree}
\begin{code}
instance Show Tree where
    show tree = treeToString tree 0

treeToString :: Tree -> Int -> String
treeToString (Leaf l _) depth = 
    branchToString depth ++ "class: " ++ (show l) ++ "\n"
treeToString (Node literal leftTree rightTree _) depth =
    let str1 = branchToString depth ++ show literal ++ "\n"
        str2 = treeToString leftTree (depth + 1)
        str3 = branchToString depth ++ "!" ++ show literal ++ "\n"
        str4 = treeToString rightTree $ depth + 1
    in str1 ++ str2 ++ str3 ++ str4

branchToString :: Int -> String
branchToString depth = "|" ++ (concat $ replicate depth "   |") ++ "--- "
\end{code}
\lstinputlisting[caption=Example of CLI output]{output/output-tree}

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
classRatio labelList = scale (1 / (norm_1 countVec)) $ countVec
    where countVec = labelCount labelList
\end{code}

\subsection{Gini Impurity}
\begin{align*}
    % \mathrm{Gini}&:\lspace^n\to\mathbb{R} \\
    \mathrm{Gini}(L)&=1-\sum_{l=0}^{L-1}p_l(L)^2=1-\|\bm{p}(L)\|_2^2
\end{align*}
\begin{code}
gini :: [Label] -> Double
gini labelList = 1.0 - (norm_2 $ classRatio labelList) ^ 2
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
        literalList = [Literal i (x !! i) | (DataPoint x _) <- dataSet]

bestSplit :: DataSet -> Split
bestSplit dataSet = myMin splitList
    where splitList = [bestSplitAtFeature dataSet f | f <- [0,1..featureNum-1]]
\end{code}

\section{Grow Tree}
\subsection{Grow Tree}
\begin{code}
growTree :: DataSet -> Int -> Int -> String -> Tree
growTree dataSet depth maxDepth nodeId =
    if stopGrowing
        then Leaf (majorLabel dataSet) nodeId
        else Node literal leftTree rightTree nodeId
    where
        literal         = sLiteral $ bestSplit dataSet
        leftTree        = growTree lData (depth + 1) maxDepth (nodeId ++ "l")
        rightTree       = growTree rData (depth + 1) maxDepth (nodeId ++ "r")
        [lData, rData]  = splitData dataSet literal
        stopGrowing =
            depth == maxDepth || 
            gini [y | (DataPoint _ y) <- dataSet] == 0 ||
            length lData == 0 || length rData == 0
\end{code}

\subsection{Stop Growing}
$$
    \operatorname{majorLabel}(D)=\underset{l\in\lspace}
    {\operatorname{argmax}}\sum_{(\bm{x},y)\in D}\mathbb{I}\left[y=l\right]
$$
\begin{code}
majorLabel :: DataSet -> Label
majorLabel dataSet = maxIndex $ labelCount [y | (DataPoint _ y) <- dataSet]
\end{code}

\newpage
\section{Output Tree in GraphViz}
\begin{code}
labelToStringForGraphViz :: Tree -> String
labelToStringForGraphViz (Leaf l leafId) =
    leafId ++ " [label=\"Class: " ++ (show l) ++ "\"]\n"
labelToStringForGraphViz (Node (Literal i v) left right nodeId) =
    nodeId ++ " [shape=box,label=\"Feature[" ++ (show i) ++ "] < " ++ (show v) ++ "\"]\n" ++
    labelToStringForGraphViz left ++ labelToStringForGraphViz right

nodeToStringForGraphViz :: Tree -> String
nodeToStringForGraphViz (Leaf _ leafId) = leafId ++ ";\n"
nodeToStringForGraphViz (Node _ left right nodeId) =
    nodeId ++ " -- " ++ nodeToStringForGraphViz left ++
    nodeId ++ " -- " ++ nodeToStringForGraphViz right

treeToStringForGraphViz :: Tree -> String
treeToStringForGraphViz tree = 
    "graph Tree {\n" ++ labelToStringForGraphViz tree ++ nodeToStringForGraphViz tree ++ "}"
\end{code}
\begin{figure}[htbp]
    \centering
    \includegraphics[width=15cm]{output/output-tree.png}
    \caption{Example of GraphViz output}
\end{figure}


\newpage
\section{Main}
\begin{code}
main :: IO()
main = do
    dataSet <- readDataFromCSV "data/iris/iris.data"
    let tree = growTree dataSet 0 10 "n"
    let treeStr = show tree
    putStrLn treeStr 
    writeFile "output/output-tree" treeStr
    writeFile "output/tree.dot" $ treeToStringForGraphViz tree
\end{code}

\section{Other Functions}
\subsection{Algorithm}
\begin{code}
myMin :: [Split] -> Split
myMin splitList = foldr min (Split (Literal 0 0) 2) splitList

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
