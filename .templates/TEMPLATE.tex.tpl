% @(#)TEMPLATE.tex.tpl

% @author (>>>USER_NAME<<<)
% Last changed: <2010-03-22 11:16:21 CET>

\documentclass[a4paper,11pt]{article}

\usepackage[english]{babel}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}
\usepackage[margin=3cm]{geometry}
\usepackage{graphicx}

% fonts
% \usepackage{palatino}
% \usepackage{mathpazo}
% \usepackage{fourier}
% \usepackage{fouriernc}
% \usepackage{lucidabr}
% \usepackage{mathptmx}

\usepackage{amsmath}
\usepackage{amsfonts,mathrsfs,amssymb}
\usepackage[squaren]{SIunits}

\usepackage{multirow}
% \usepackage{hyperref}
% \usepackage{floatflt}
\usepackage{algorithm}
\usepackage{algpseudocode}
% \usepackage{algorithmic}
% \usepackage{clrscode}
\usepackage{verbatim}
\usepackage{moreverb}

\newcommand{\fgref}[1]{Figure \ref{#1}}
\newcommand{\tbref}[1]{Table \ref{#1}}
\newcommand{\alref}[1]{Algorithm \ref{#1}}

% math stuff
\newcommand{\ve}[1]{\mathbf{#1}}
\newcommand{\ordo}[1]{\mathcal{O}(#1)}
\newcommand{\degrees}{{}^\circ}

% Macron för derivator
\newcommand{\de}{\mathrm{d}}
\newcommand{\pd}{\partial}
\newcommand{\dd}[2]{\frac{\de#1}{\de#2}}
\newcommand{\pdd}[2]{\frac{\partial #1}{\partial #2}}
\newcommand{\pddc}[3]{ \left( \frac{\partial #1}{\partial #2} \right)_#3 }
\newcommand{\ddn}[3]{\frac{\de^{#1} #2}{\de #3^{#1}}}
\newcommand{\pddn}[3]{\frac{\partial^{#1} #2}{\partial #3^{#1}}}
\newcommand{\ddop}[1]{\frac{\de}{\de #1}}
\newcommand{\pddop}[1]{\frac{\partial}{\partial #1}}
\newcommand{\ddopn}[2]{\frac{\de^{#1}}{\de #2^{#1}}}
\newcommand{\pddopn}[2]{\frac{\partial^{#1}}{\partial #2^{#1}}}

\newcommand{\verbinpt}[1]{{\scriptsize \verbatiminput{#1}}}
\newcommand{\includescript}[1]{{\scriptsize\verbatimtabinput[4]{#1}}}

\setlength{\parindent}{0pt}
\setlength{\parskip}{2ex plus 0.5ex minus 0.2ex}
% \thispagestyle{empty}
\pagestyle{headings}

\title{}
\author{Karl Ljungkvist\\ 861012-1439\\kalj0193@student.uu.se}
\date{}

\begin{document}

\maketitle

(>>>POINT<<<)

% \begin{figure}[ht]
%   \centering
%   \includegraphics[width=\textwidth]{}
%   \caption{}
%   \label{}
% \end{figure}

\end{document}
