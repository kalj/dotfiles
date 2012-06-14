% @(#)TEMPLATE.tex.tpl
% @author (>>>USER_NAME<<<) <(>>>AUTHOR<<<)>

\documentclass[a4paper,10pt]{article}
% \documentclass[]{beamer}
% \usetheme{UUIT}
% \setbeamertemplate{navigation symbols}{} % Disable navigation symbols

\usepackage[english]{babel}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}

%% Layout
% \usepackage{fullpage}
% \usepackage{geometry}
% \pagestyle{headings}

%% Have new paragraph begin with blank line rather than indent.
% \setlength{\parindent}{0pt}
% \setlength{\parskip}{2ex plus 0.5ex minus 0.2ex}

%% math
\usepackage{amsmath,amsfonts,mathrsfs,amssymb}
\usepackage[squaren]{SIunits}
% \usepackage{ulem}
% \renewcommand{\emph}{\textit}

%% fonts
% \usepackage{palatino}
% \usepackage{soul} % to widen text

%% figure-related stuff
\usepackage{graphicx}
\usepackage{subfigure}
\usepackage{multirow}
\usepackage{hyperref}
% \usepackage{floatflt}

%% Code inclusion evironments
\usepackage{comment} % for orgmode tables
\usepackage{moreverb}
\newcommand{\verbinpt}[1]{{\scriptsize\verbatimtabinput[4]{#1}}}
\usepackage{algorithm}
\usepackage{algpseudocode}
\usepackage{color}
\usepackage{listings}

\definecolor{dkgreen}{rgb}{0,0.6,0}
\definecolor{gray}{rgb}{0.5,0.5,0.5}

%% Matlab settings
\lstset{language=Matlab,
  keywords={break,case,catch,continue,else,elseif,end,for,function,
    global,if,otherwise,persistent,return,switch,try,while},
  basicstyle=\footnotesize\ttfamily,
  keywordstyle=\color{blue},
  commentstyle=\color{red},
  stringstyle=\color{dkgreen},
  % numbers=left,
  numberstyle=\tiny\color{gray},
  stepnumber=1,
  numbersep=10pt,
  backgroundcolor=\color{white},
  tabsize=2,
  showspaces=false,
  showstringspaces=false}

%% C++ settings
% \lstset{language=C++
%   ,morekeywords={for, if, else, and, while} %, __device__, __global__, __shared__}
%   % ,otherkeywords={<, >, (,),;}
%   ,emph={(,),;}
%   ,emphstyle=\bfseries\color{dkgreen}
%   ,basicstyle=\bfseries\footnotesize\ttfamily
%   ,keywordstyle=\color{blue}
%   ,commentstyle=\color{dkgreen}
%   ,frame=tb
%   ,mathescape=true
%   ,captionpos=b
%   % ,numbers=left
%   ,numbersep=10pt
% }


%% Misc.
\usepackage{url}
% footnote with asterisk, dagger, etc.
\long\def\symbolfootnote[#1]#2{\begingroup%
\def\thefootnote{\fnsymbol{footnote}}\footnote[#1]{#2}\endgroup}

%% References
\newcommand{\fgref}[1]{\autoref{#1}}
\newcommand{\tbref}[1]{\autoref{#1}}
\newcommand{\alref}[1]{Algorithm \ref{#1}}
\newcommand{\secref}[1]{Section \ref{#1}}
\newcommand{\appref}[1]{Appendix \ref{#1}}
\newcommand{\liref}[1]{Listing \ref{#1}}

%% math macros
\newcommand{\ve}[1]{\ensuremath{\mathbf{#1}}}
% \newcommand{\mat}[1]{\uuline{#1}}
\newcommand{\ordo}[1]{\ensuremath{\mathcal{O}(#1)}}
\newcommand{\degrees}{\ensuremath{{}^\circ}}
\newcommand{\de}{\ensuremath{\mathrm{d}}}
\newcommand{\pd}{\ensuremath{\partial}}
\newcommand{\dd}[2]{\ensuremath{\frac{\de#1}{\de#2}}}
\newcommand{\pdd}[2]{\ensuremath{\frac{\partial #1}{\partial #2}}}
\newcommand{\pddc}[3]{\ensuremath{ \left( \frac{\partial #1}{\partial #2} \right)_#3 }}
\newcommand{\ddn}[3]{\ensuremath{\frac{\de^{#1} #2}{\de #3^{#1}}}}
\newcommand{\pddn}[3]{\ensuremath{\frac{\partial^{#1} #2}{\partial #3^{#1}}}}
\newcommand{\ddop}[1]{\ensuremath{\frac{\de}{\de #1}}}
\newcommand{\pddop}[1]{\ensuremath{\frac{\partial}{\partial #1}}}
\newcommand{\ddopn}[2]{\ensuremath{\frac{\de^{#1}}{\de #2^{#1}}}}
\newcommand{\pddopn}[2]{\ensuremath{\frac{\partial^{#1}}{\partial #2^{#1}}}}
\newcommand{\R}{\ensuremath{\mathbb{R}}}
\newcommand{\inprod}[2]{\ensuremath{\left(#1,#2\right)}}

%% Defining the authors email address
\newcommand{\insertemail}{karl.ljungkvist@it.uu.se}%


% \thispagestyle{empty}

\title{}
\author{(>>>USER_NAME<<<) \\(>>>AUTHOR<<<)}
\date{}

\begin{document}

\maketitle

(>>>POINT<<<)

\end{document}
