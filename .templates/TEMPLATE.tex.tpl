% @(#)TEMPLATE.tex.tpl
% @author (>>>USER_NAME<<<) <(>>>AUTHOR<<<)>

% -----------------------------------------------------------------------------
% Preamble
% -----------------------------------------------------------------------------

\documentclass[a4paper,10pt]{article}
% \documentclass[]{beamer}
% \usetheme{UUIT}
% \setbeamertemplate{navigation symbols}{} % Disable navigation symbols

\usepackage[english]{babel}
\usepackage[utf8]{inputenc}
\usepackage[T1]{fontenc}

%% Layout
% \usepackage{fullpage}

%% Have new paragraph begin with blank line rather than indent.
% \setlength{\parindent}{0pt}
% \setlength{\parskip}{2ex plus 0.5ex minus 0.2ex}

%% math
\usepackage{amsmath,amsfonts,mathrsfs,amssymb}
\usepackage[squaren]{SIunits}

%% figure-related stuff
\usepackage{graphicx}
\usepackage{subfigure}
\usepackage{multirow}
\usepackage{hyperref}
% \usepackage{floatflt}

%% Algorithms
\usepackage{algorithm}
\usepackage{algpseudocode}

%% Code inclusion evironments
\usepackage{color}
\usepackage{listings}
\usepackage{xcolor}
\xdefinecolor{codegreen}{RGB}{0,128,0}
\xdefinecolor{codedgreen}{RGB}{0,64,0}
\xdefinecolor{codegrey}{RGB}{128,128,128}
\xdefinecolor{codeblue}{RGB}{0,0,128}
\xdefinecolor{codered}{RGB}{100,0,0}

% Matlab settings
\lstdefinestyle{mymatlab}{
  language=Matlab
  ,keywords={break,case,catch,continue,else,elseif,end,for,function,
    global,if,otherwise,persistent,return,switch,try,while}
  ,basicstyle=\bfseries\scriptsize\ttfamily
  ,keywordstyle=\color{codeblue}
  ,commentstyle=\color{codered}
  ,stringstyle=\color{codegreen}
  ,numbers=left
  ,stepnumber=1
  ,numbersep=10pt
  ,tabsize=2
  ,showspaces=false
  ,showstringspaces=false
  ,frame=tb
  ,xleftmargin=\parindent
}

% C++ settings
\lstdefinestyle{mycpp}{
  language=C++
  ,basicstyle=\bfseries\scriptsize\ttfamily
  ,stringstyle=\color{codered}
  ,morekeywords={for, if, else, while,std} %, __device__, __global__, __shared__}
  ,keywordstyle=\color{codeblue}
  ,commentstyle=\color{codegreen}
  ,showstringspaces=false
  ,frame=tb
  ,xleftmargin=\parindent
%   ,mathescape=true
%   ,captionpos=b
%   % ,numbers=left
%   ,numbersep=10pt
}



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
\newcommand{\R}{\ensuremath{\mathbb{R}}}
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
\newcommand{\inprod}[2]{\ensuremath{\left(#1,#2\right)}}

%% Defining the authors email address
\newcommand{\insertemail}{karl.ljungkvist@it.uu.se}%

% -----------------------------------------------------------------------------
% End of preamble
% -----------------------------------------------------------------------------


% \thispagestyle{empty}

\title{}
\author{(>>>USER_NAME<<<) \\(>>>AUTHOR<<<)}
\date{}

\begin{document}

\maketitle

(>>>POINT<<<)

\end{document}
