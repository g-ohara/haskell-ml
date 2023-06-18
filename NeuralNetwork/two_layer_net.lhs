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
\title{2-Layer Neural Network}
\author{Genji Ohara}
\maketitle

% \section{What does the code mean?}
\section{Preamble}
\begin{code}
import Numeric.LinearAlgebra
import Prelude hiding ((<>))

type Vec = Vector R
type Mat = Matrix R
\end{code}

\section{Constants}
\begin{code}
inputSize   :: Int
hiddenSize  :: Int
outputSize  :: Int
inputSize   = 784
hiddenSize  = 50
outputSize  = 10

w1_start    :: Int
w1_size     :: Int
w2_start    :: Int
w2_size     :: Int
b2_start    :: Int
weight_size :: Int
w1_start    = 0
w1_size     = inputSize * hiddenSize
w2_start    = w1_size + hiddenSize
w2_size     = hiddenSize * outputSize
b2_start    = w2_start + w2_size
weight_size = w1_size + hiddenSize + w2_size + outputSize
\end{code}

\section{Layers}
\subsection{Affine}
\subsubsection{forward}
\begin{code}
affine :: Mat -> Vec -> Mat -> Mat
affine w b x = x <> w + asRow b

affineDX :: Mat -> Mat -> Mat
affineDX w dout = dout <> (tr w)

affineDW :: Mat -> Mat -> [R]
affineDW x dout = (matToList $ (tr x) <> dout) ++ (toList $ sum $ toRows dout)
\end{code}

\subsection{Activation Function}
\subsubsection{ReLU}
\begin{align*}
    \mathrm{ReLU}(x)&=\max(x,0) \\
    \mathrm{ReLU}(X)&=
    \begin{bmatrix}
        \mathrm{ReLU}(x_{11})&\cdots&\mathrm{ReLU}(x_{1N}) \\
        \vdots&\ddots&\vdots\\
        \mathrm{ReLU}(x_{N1})&\cdots&\mathrm{ReLU}(x_{NN}) \\
    \end{bmatrix}
\end{align*}
\begin{code}
relu :: Mat -> Mat
relu = cmap (max 0)

reluBackward :: Mat -> Mat -> Mat
reluBackward dout x = dout * mask
    where mask = cmap (\_x -> if _x > 0 then 1 else 0) x
\end{code}

\subsubsection{Sigmoid}
$$ \mathrm{sigmoid}(x)=\frac{1}{1+e^{-x}} $$
\begin{code}
sigmoid :: R -> R
sigmoid x = 1 / (1 + exp(-x))
\end{code}

\newpage

\subsection{Cross Entropy Error}
\begin{align*}
    \mathrm{CEE}(\bm{y},\bm{t})&=-\bm{t}^T
    \begin{bmatrix}\ln y_1\\\vdots\\\ln y_D\end{bmatrix} \\
    \mathrm{CEE}(Y,T)&=\sum_{i=1}^N\mathrm{CEE}(\bm{y}_i,\bm{t}_i)
\end{align*}
\begin{code}
sumCrossEntropyError :: [Vec] -> [Vec] -> R
sumCrossEntropyError [] _ = 0
sumCrossEntropyError _ [] = 0
sumCrossEntropyError (y:ys) (t:ts) = -t <.> (cmap log y) + sumCrossEntropyError ys ts

crossEntropyError :: Mat -> Mat -> R
crossEntropyError y t = sumCrossEntropyError ys ts / batchSize
    where
        ys = toRows y
        ts = toRows t
        batchSize = fromIntegral $ length ys
\end{code}
\subsection{Softmax}
\subsubsection{Softmax}
\begin{align*}
    \exp(\bm{x})&=\begin{bmatrix}e^{x_1}\\\vdots\\e^{x_N}\end{bmatrix} \\
    \mathrm{softmax}(\bm{x})&=\frac{\exp(\bm{x})}{\|\exp(\bm{x})\|_1}
    =\frac{\exp(\bm{x}-\bm{c})}{\|\exp(\bm{x}-\bm{c})\|_1} \\
    \mathrm{softmax}(X)&=\begin{bmatrix}\mathrm{softmax}(\bm{x}_{:1})&
    \cdots&\mathrm{softmax}(\bm{x}_{:N})\end{bmatrix}
\end{align*}
\begin{code}
softmaxVec :: Vec -> Vec
softmaxVec xVec = scale (1 / norm_1 expVec) expVec
    where
        c       = maxElement xVec
        cVec    = vector $ take (size xVec) [c,c..]
        expVec  = cmap exp $ xVec - cVec

softmax :: Mat -> Mat
softmax = fromRows . (map softmaxVec) . toRows
\end{code}

\subsubsection{Softmax with Loss}
\begin{code}
softmaxWithLoss :: Mat -> Mat -> R
softmaxWithLoss x t = crossEntropyError (softmax x) t

softmaxWithLossBackward :: Mat -> Mat -> Mat
softmaxWithLossBackward y t = (y - t) / (scalar $ fromIntegral $ rows y)
\end{code}

\subsection{Loss Function}
\begin{align*}
    \mathcal{L}(\bm{w};X,T)&=\mathrm{softmaxWithLoss}(\hat{Y},T)\\
    &=\mathrm{CEE}(\mathrm{softmax}(\hat{Y}),T)
\end{align*}
\begin{code}
loss :: Vec -> Mat -> Mat -> R
loss w x t = softmaxWithLoss (forwardProp w x) t
\end{code}

\subsection{One-Hot Vector}
\begin{code}
oneHotList :: Int -> Int -> [R]
oneHotList len idx =
    if len == 0
        then []
        else
            if idx == 0
                then 1 : oneHotList (len - 1) (idx - 1)
                else 0 : oneHotList (len - 1) (idx - 1)

oneHotVector :: Int -> Int -> Vec
oneHotVector len idx = vector $ oneHotList len idx

oneHotMat :: Int -> [Int] -> Mat
oneHotMat len labelList = fromRows $ map (oneHotVector len) labelList
\end{code}

\subsection{Forward Propagetion}
\begin{code}
-- 出力層の活性化関数(softmax)を適用する直前まで計算
forwardProp :: Vec -> Mat -> Mat
forwardProp weight x = affine w2 b2 $ relu $ affine w1 b1 x
    where
        w1 = reshape hiddenSize $ subVector w1_start w1_size weight :: Mat
        w2 = reshape outputSize $ subVector w2_start w2_size weight :: Mat
        b1 = subVector w1_size  hiddenSize weight
        b2 = subVector b2_start outputSize weight
\end{code}

\subsection{Prediction}
\begin{code}
predict :: Vec -> Mat -> Mat
predict w x = oneHotMat outputSize $ map maxIndex $ toRows $ forwardProp w x
\end{code}


\subsection{Gradient}
\begin{code}
numericalGradientList :: Int -> (Vec -> R) -> Vec -> [R]
numericalGradientList idx f x =
    if idx == size x
        then []
        else
            let h  = 1e-4
                dx = cmap (* h) $ oneHotVector (size x) idx
                x1 = x + dx
                x2 = x - dx
            in (f(x1) - f(x2)) / (2 * h) : numericalGradientList (idx + 1) f x

numericalGradient :: (Vec -> R) -> Vec -> Vec
numericalGradient f = vector . (numericalGradientList 0 f)

matToList :: Mat -> [R]
matToList = concat . toLists

gradient :: Vec -> Mat -> Mat -> Vec
gradient weight x t =

    let w1 = reshape hiddenSize $ subVector w1_start w1_size weight :: Mat
        w2 = reshape outputSize $ subVector w2_start w2_size weight :: Mat
        b1 = subVector w1_size  hiddenSize weight
        b2 = subVector b2_start outputSize weight

        -- forward propagation
        a1 = affine w1 b1 x
        y1 = relu a1
        y2 = softmax $ affine w2 b2 y1

        -- backward propagation
        da2 = softmaxWithLossBackward y2 t
        dx2 = affineDX w2 da2
        dw2 = affineDW y1 da2
        da1 = reluBackward dx2 a1
        dw1 = affineDW x da1

    in fromList $ dw1 ++ dw2
--

gradientCheck :: Vec -> Mat -> Mat -> R
gradientCheck w x t =
    let num_grad    = numericalGradient (\_w -> loss _w x t) w
        grad        = gradient w x t
        err_sum     = sum $ map abs $ toList $ num_grad - grad
    in err_sum / (fromIntegral $ length $ toList grad)
\end{code}

\subsection{Learning}
\begin{code}
learn :: Vec -> Mat -> Mat -> R -> R -> Vec
learn weight x t learningRate iterNum =
    if iterNum == 0
        then weight
        else learn new_w x t learningRate (iterNum - 1)
            where
                new_w = weight - (cmap (* learningRate) $ gradient weight x t)

testAccuracy :: Vec -> Mat -> Mat -> R
testAccuracy w x t = scoreSum / (fromIntegral $ rows x)
    where scoreSum = sumElements $ takeDiag $ (predict w x) <> (tr t)

main :: IO ()
main = do
    cs1 <- readFile "dataset/train_data.dat"
    cs2 <- readFile "dataset/test_data.dat"
    cs3 <- readFile "dataset/train_label.dat"
    cs4 <- readFile "dataset/test_label.dat"

    let batchSize  = 100

    let trainDataList   = map read $ lines cs1
    let testDataList    = map read $ lines cs2
    let trainLabelList  = map read $ lines cs3
    let testLabelList   = map read $ lines cs4

    weight <- flatten <$> randn 1 weight_size

    let trainData   = matrix inputSize $ take (batchSize * inputSize) trainDataList
    let testData    = matrix inputSize testDataList
    let trainLabel  = oneHotMat outputSize $ take batchSize trainLabelList
    let testLabel   = oneHotMat outputSize testLabelList

    putStr "Now Loading Training Data...\n"
    putStr "size of train data  : "
    print $ size trainData
    putStr "size of train label : "
    print $ size trainLabel
    putStr "size of test data   : "
    print $ size testData
    putStr "size of test label  : "
    print $ size testLabel

    -- putStr "Gradient Check      : "
    -- print $ gradientCheck weight x t

    let learningRate = 0.1
    let iterNum = 100
    let newW = learn weight trainData trainLabel learningRate iterNum

    print $ testAccuracy newW trainData trainLabel
    print $ testAccuracy newW testData testLabel
\end{code}
\end{document}