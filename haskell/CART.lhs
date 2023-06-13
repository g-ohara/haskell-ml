\documentclass[dvipdfmx,11pt]{article}
\usepackage{listings}
\usepackage{color}
\usepackage{bm}
% \usepackage{amsmath}
\usepackage{amssymb}

\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
        \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}

\lstset
{
    basicstyle=\small\ttfamily,
    flexiblecolumns=false,
    basewidth={0.5em,0.45em},
    literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
        {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
        {\\\\}{{\char`\\\char`\\}}1
        {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
        {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2
        {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
        {>>}{{>>}}2 {>>=}{{>>=}}2
        {|}{{$\mid$}}1,
    frame=single,
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
    lineskip=-0.5ex,
    tabsize=4,
    escapeinside={(@}{@)}
}

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

import DataSet
\end{code}

\section{Data Type Definition}
\begin{code}
type DataSet = [DataPoint]

data Literal = Literal{lFeatureIdx :: Int, lValue :: Double} deriving Show

data Split = Split {sLiteral :: Literal, sScore :: Double} deriving Show

instance Eq Split where
    (Split l s) == (Split l' s') = (Prelude.==) s s'
-- (Split l s) /= (Split l' s') = (Prelude./=) s s'

instance Ord Split where
    (Split l s) <= (Split l' s') = (Prelude.<=) s s'

data Tree = 
    Leaf {label :: Int} | 
    Node {literal :: Literal, left :: Tree, right :: Tree}
    deriving Show
\end{code}

\section{Gini Impurity}
\begin{code}
gini :: DataSet -> Double
gini points = 1.0 - (sum $ map (^ 2) pList)
    where
        pList       = map (/ dataSize) cntList
        dataSize    = fromIntegral $ length points
        cntList     = map fromIntegral
            [length $ filter (\x -> (Prelude.==) (dLabel x) 0) points,
            length $ filter (\x -> (Prelude.==) (dLabel x) 1) points,
            length $ filter (\x -> (Prelude.==) (dLabel x) 2) points]
\end{code}

\section{Split Data}
\begin{code}
splitData :: DataSet -> Literal -> [DataSet]
splitData dataSet literal = [lData, rData]
    where
        lCondition = \x -> ((dFeature x) !! lFeatureIdx literal) <   (lValue literal)
        rCondition = \x -> ((dFeature x) !! lFeatureIdx literal) >=  (lValue literal)
        lData = filter lCondition dataSet
        rData = filter rCondition dataSet

scoreLiteral :: DataSet -> Literal -> Split
scoreLiteral dataSet literal = Split literal score
    where
        score = sum $ map (\x -> (gini x) * (fromIntegral $ length x) / dataSize) splittedData
        dataSize = fromIntegral $ length dataSet
        splittedData = splitData dataSet literal

bestSplitAtGivenFeature :: DataSet -> Int -> Split
bestSplitAtGivenFeature dataSet featureIdx = maximum splitList
    where
        splitList = map (scoreLiteral dataSet) literalList :: [Split]
        literalList = map (Literal featureIdx) $ valueList
        valueList = map (\x -> (dFeature x) !! featureIdx) dataSet

bestSplit :: DataSet -> Split
bestSplit dataSet = maximum $ map (bestSplitAtGivenFeature dataSet) [0,1..featureNum-1]
\end{code}

\section{Main}
\begin{code}
main = do
    rawDataSet <- parseFromFile csvFile "../data/iris/iris.data"
    let dataSet = either (\x -> []) processData rawDataSet
    print $ bestSplit dataSet
\end{code}

\end{document}
