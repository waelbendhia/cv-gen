%% The MIT License (MIT)
%%
%% Copyright (c) 2015 Daniil Belyakov
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

\ProvidesClass{waelcv}[2023/10/23 v1.0.0 waelcv CV class]

% Set up the fonts according to options
\def\mainfontsize{11pt}
\def\mainfontface{Iosevka Nerd Font Propo}

\DeclareOption{calibri}{%
  \def\mainfontface{Calibri}
}

% Set up papersize according to options, default to US letter
\def\papersize{letterpaper}
%Set up bool for adjusting vspacing later
\newif\ifpapersize

\DeclareOption{a4paper}{%
  \def\papersize{a4paper}
  \papersizetrue
}

\DeclareOption{letterpaper}{%
  \def\papersize{letterpaper}
  \papersizefalse
}
\ProcessOptions\relax

% Set font size and paper type
\LoadClass[\papersize,\mainfontsize]{article}

% Disable wordbreaks
\usepackage[none]{hyphenat}

% For HyperLinks
\usepackage{hyperref}

% Color stuff
\usepackage{xcolor}
\definecolor{blue}{HTML}{1e66f5}
\definecolor{white}{HTML}{eff1f5}
\definecolor{black}{HTML}{4c4f69}
\definecolor{subheading}{HTML}{5c5f77}
\pagecolor{white}
\color{black}
\hypersetup{
    colorlinks,
    linkcolor={blue!50!black},
    citecolor={blue!50!black},
    urlcolor={blue!80!black}
}

% Set document margins
\usepackage[left=0.75in,top=0.6in,right=0.75in,bottom=0.6in]{geometry}

% Set font face
\usepackage{fontspec}
\setmainfont[SmallCapsFeatures={Renderer=Basic},
Ligatures={TeX, NoCommon, NoDiscretionary}]{\mainfontface}

% Remove paragraph indentation
\usepackage[parfill]{parskip}

% Required for boldface (\bf and \bfseries) tabular columns
\usepackage{array}

% Required for ifthenelse statements
\usepackage{etoolbox}

% Suppress page numbers
\pagestyle{empty}

% Flexible tables
\usepackage{tabu}

% For custom margins
\usepackage{changepage}

% To customize lists
\usepackage{enumitem}
% Remove left margin from lists, suppress vertical item separation and set top
% separation to a negative value to suppress a single parskip before lists
\setlist{leftmargin=*, label={}}

% For adjusting spacing based on the platform used
\usepackage{ifthen}
\usepackage{ifplatform}

% For spcaed small caps
\usepackage{microtype}

\DeclareMicrotypeSet*{smallcapsi} {
  encoding = {OT1,T1,T2A,LY1,OT4,QX,T5,TS1,EU1,EU2},
  shape = {sc*,si,scit}
}

%--------------------------------------------------------------------------------
%                                   Constants                                   -
%--------------------------------------------------------------------------------

\ifthenelse{\equal{\platformname}{\notwindowsname}}{
  \def\namespaceskip{0.575em}
  \def\afterheaderspace{1.1pt}
  \def\beforesectionheaderspace{5pt}
  \def\sectionheadermargin{6pt}
  \def\beforesectionheaderhrulespace{2.85pt}
  \def\sectionheaderhrlueheight{0.518pt}
  \def\aftersectionheaderspace{0pt}
  % \def\aftersectionheaderspace{-5.8pt}

  \ifpapersize
    \def\aftersinglelinesubsectionheaderspace{-5.75pt}
  \else
    \def\aftersinglelinesubsectionheaderspace{-20.25pt}
  \fi

  \def\afterdoublelinesubsectionheaderspace{-11.75pt}
  \def\aftermultilinesubsectionheaderspace{-7.75pt}
  \def\afteremptysubsectionheaderspace{1.25pt}
  \def\subsectionmargin{9pt}
  \def\projectmargin{12pt}
  \def\aftersubsectionspace{2.1pt}
}{
  \def\namespaceskip{0.4em}
  \def\afterheaderspace{3pt}
  \def\beforesectionheaderspace{4.75pt}
  \def\sectionheadermargin{6pt}
  \def\beforesectionheaderhrulespace{3pt}
  \def\sectionheaderhrlueheight{0.5pt}
  \def\aftersectionheaderspace{-4pt}

  \ifpapersize
    \def\aftersinglelinesubsectionheaderspace{-4pt}
  \else
    \def\aftersinglelinesubsectionheaderspace{-18.5pt}
  \fi

  \def\afterdoublelinesubsectionheaderspace{-10pt}
  \def\aftermultilinesubsectionheaderspace{-6pt}
  \def\afteremptysubsectionheaderspace{3pt}
  \def\subsectionmargin{9pt}
  \def\aftersubsectionspace{4pt}
}

%--------------------------------------------------------------------------------
%                            Header Setup and Printing                          -
%--------------------------------------------------------------------------------

% Address
\makeatletter

\newcommand\address[1]{\def\@address{#1}}
\address{}

\newcommand\phonenumber[1]{\def\@phonenumber{#1}}
\phonenumber{}

% Name
\newcommand\name[1]{\def\@name{#1}}
\name{}

\newcommand\printname{
  \typeout {kokoko}
  \typeout \spaceskip
  \spaceskip \namespaceskip \relax
  \textbf{\LARGE\textls[110]{\textsc{\@name}}}
  \linebreak
  \@phonenumber
  \linebreak
  \@address
}

\makeatother

\makeatletter

% GitHub
\newcommand\github[1]{\def\@github{#1}}
\github{}

% Email
\newcommand\email[1]{\def\@email{#1}}
\email{}

% LinkedIn
\newcommand\linkedin[1]{\def\@linkedin{#1}}
\linkedin{}


% \newcommand\printcontacts{
%   \small{\textbf{Email:}\nolinebreak\space\href{mailto:\@email}{\@email}}
%   \linebreak
%   \small{\textbf{LinkedIn:}\nolinebreak\space\href{https://linkedin.com/in/\@linkedin}{\@linkedin}}
%   \linebreak
%   \small{\textbf{GitHub:}\nolinebreak\space\href{https://github.com/\@github}{\@github}}
% }

\newcommand\printcontacts{
  \begin{small}
  \begin{tabu} to 1\linewidth { X[r,m] X[l,m] }
    \textbf{Email}    & \href{mailto:\@email}{\@email} \\
    \textbf{LinkedIn} & \href{https://linkedin.com/in/\@linkedin}{\@linkedin} \\
    \textbf{GitHub}   & \href{https://github.com/\@github}{\@github} \\
  \end{tabu}
  \end{small}
}

\makeatother

\makeatletter
\newcommand\makeheader{
  \begin{center}
    \begin{tabu} to 1\textwidth { X[l,m] X[r,m] }
      \printname & \printcontacts \\
    \end{tabu}
  \end{center}
  \vspace*{\afterheaderspace}
}
\makeatother

%--------------------------------------------------------------------------------
%                            Sections and Subsections                           -
%--------------------------------------------------------------------------------

% IntroBlock
\makeatletter
\newenvironment{summary}[0]{
  \vspace*{\beforesectionheaderspace}
  \vspace*{\beforesectionheaderspace}
}{}
\makeatother

% Lists
\makeatletter
\newcommand{\rowlist}[2]{
  \textbf{\color{subheading}#1: }{#2}
}{}
\makeatother

% Print a section header
\makeatletter
\newenvironment{cvsection}[1]{
  \vspace*{\beforesectionheaderspace}
  % Set text margins to equal \tabcolsep (6pt by default)
  \begin{adjustwidth}{\sectionheadermargin}{\sectionheadermargin}
    \textsc{\color{subheading}\textbf{#1}}
  \end{adjustwidth}
  \vspace*{\beforesectionheaderhrulespace}
  \hrule height \sectionheaderhrlueheight
  \vspace*{\aftersectionheaderspace}
}{}
\makeatother


% Project body
\makeatletter
\newenvironment{project}[2]{
  \par
  \begin{tabu} to 1\linewidth { X[l,p] X[r,p] }
    \textsc{\color{subheading}\textbf{#1}} & #2 \\
  \end{tabu}
  \vspace*{\aftersinglelinesubsectionheaderspace}
  \begin{adjustwidth}{\projectmargin}{\projectmargin}
}{
  \end{adjustwidth}
}
\makeatother

% Print a subsection
\makeatletter
% Define toggles checking if titles were defined
\newtoggle{lefttitledefined}
\newtoggle{centertitledefined}
\newtoggle{righttitledefined}

\newenvironment{cvsubsection}[4][1]{
  \notblank{#2}{\toggletrue{lefttitledefined}}{}
  \notblank{#3}{\toggletrue{centertitledefined}}{}
  \notblank{#4}{\toggletrue{righttitledefined}}{}
  \ifboolexpr{togl {lefttitledefined} or togl {centertitledefined} or togl {righttitledefined}}{
    {
      \color{subheading}
      \begin{tabu} to 1\textwidth { X[l,p] X[c,p] X[r,p] }
        \textbf{#2} & \textbf{#3} & \textbf{#4} \\
      \end{tabu}
    }
    % Add space according to the specidied number of lines
    \ifnumcomp{#1}{=}{1}{\vspace*{\aftersinglelinesubsectionheaderspace}}{
      \ifnumcomp{#1}{=}{2}{\vspace*{\afterdoublelinesubsectionheaderspace}}{
        \vspace*{\aftermultilinesubsectionheaderspace}
      }
    }
  }{
    \vspace*{\afteremptysubsectionheaderspace}
  }
  \togglefalse{lefttitledefined}
  \togglefalse{centertitledefined}
  \togglefalse{righttitledefined}
  \begin{adjustwidth}{\subsectionmargin}{\subsectionmargin}
}
{
  \end{adjustwidth}
  \vspace*{\aftersubsectionspace}
}
\makeatother

\sloppy
