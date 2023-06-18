\documentclass[dvipdfmx,11pt]{article}
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
    \lstset{
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
	keywordstyle={\color[rgb]{0,0,1}},
	stringstyle={\color[rgb]{0.6,0,0.6}},
	commentstyle={\color[rgb]{0,0.5,0}},
	frame=single,
	frameround=tttt, % makes four corners round
	framesep=10pt,
	breaklines=true,
	columns=fixed,
	numbers=none,
	xleftmargin=0zw,
	xrightmargin=0zw,
	numberstyle={\scriptsize},
	stepnumber=1,
	lineskip=-0.5ex,
	tabsize=2,
	escapeinside={(@}{@)}
    }

\begin{document}
\title{Ridge Regression}
\author{Genji Ohara}
\maketitle
\section{What does the code mean?}
\subsection{Preamble}
\begin{code}
module Ridge where

import Numeric.LinearAlgebra
import Prelude hiding ((<>))

type Vec = Vector R
type Mat = Matrix R
\end{code}

\subsection{Adding Bias}
$$
  \tilde{\bm{X}}\triangleq\left[\ \begin{matrix}
  1 \\ 1 \\ \vdots \\ 1\end{matrix}\ \ \bm{X}\ \right],\quad
  \tilde{\bm{w}}\triangleq\begin{bmatrix}w_0\\ \bm{w}\end{bmatrix}
$$
\begin{code}
addBias :: Mat -> Mat
addBias x = fromColumns $ [bias] ++ (toColumns x)
    where bias = vector $ take (rows x) [1,1..]
\end{code}

\newpage
\subsection{Prediction}
$$
  \hat{\bm{y}}=\begin{bmatrix}w_0 \\ w_0 \\ \vdots \\ w_0\end{bmatrix}
  +\bm{X}\bm{w}=\tilde{\bm{X}}\tilde{\bm{w}}
$$
\begin{code}
predict :: Vec -> Vec -> R
predict w x = w <.> (vector $ [1.0] ++ toList x)
\end{code}

\subsection{Fitting}
We minimize the objective:
$$ E(\tilde{\bm{w}})=\|\bm{y}-\tilde{\bm{X}}\tilde{\bm{w}}\|^2+\lambda\|\tilde{\bm{w}}\|^2 $$
Gradient:
$$
  \nabla E(\tilde{\bm{w}})=2\left[\left(\tilde{\bm{X}}^T\tilde{\bm{X}}+\lambda I\right)
  \tilde{\bm{w}}-\tilde{\bm{X}}^T\bm{y}\right]
$$
Therefore
$$
  \underset{\tilde{\bm{w}}} {\operatorname{argmin}}\ E(\tilde{\bm{w}})=\left(
  \tilde{\bm{X}}^T\tilde{\bm{X}}+\lambda I\right)^{-1}\tilde{\bm{X}}^T\bm{y}
$$
\begin{code}
fit :: Mat -> Vec -> R -> Vec
fit x_til y lambda = (inv a) #> ((tr x_til) #> y)
  where a = (tr x_til) <> x_til + (scale lambda $ ident $ cols x_til)
\end{code}


\end{document}