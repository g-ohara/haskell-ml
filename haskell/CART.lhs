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
    (Split l s) == (Split l' s') = (Prelude.==) s s'

instance Ord Split where
    (Split l s) <= (Split l' s') = (Prelude.<=) s s'

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
\section{Split Data}
\begin{align*}
    D_l(i,v)&=\left\{(\bm{x},y)\in D\mid x_i<v\right\} \\
    D_r(i,v)&=\left\{(\bm{x},y)\in D\mid x_i\ge v\right\}
\end{align*}

\begin{code}
splitData :: DataSet -> Literal -> [DataSet]
splitData dataSet literal = [lData, rData]
    where
        lData = [(DataPoint x y) | (DataPoint x y) <- dataSet, x !! i <  v]
        rData = [(DataPoint x y) | (DataPoint x y) <- dataSet, x !! i >= v]
        i = lFeatureIdx literal
        v = lValue literal

scoreLiteral :: DataSet -> Literal -> Split
scoreLiteral dataSet literal = Split literal score
    where
        score = sum $ map (\x -> (gini $ map dLabel x) * (fromIntegral $ length x) / dataSize) splittedData
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
