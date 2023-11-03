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

\ProcessOptions\relax

% Set font size and paper type
\LoadClass[a4paper,\mainfontsize]{article}

% Disable wordbreaks
\usepackage[none]{hyphenat}

% For HyperLinks
\usepackage{hyperref}

% Color stuff
\usepackage{xcolor}
\definecolor{highlight}{HTML}{1e66f5}
\definecolor{background}{HTML}{eff1f5}
\definecolor{foreground}{HTML}{4c4f69}
\definecolor{subheading}{HTML}{5c5f77}
\pagecolor{background}
\color{foreground}
\hypersetup{
	colorlinks,
	linkcolor={highlight!50!foreground},
	citecolor={highlight!50!foreground},
	urlcolor={highlight!80!foreground}
}

% Set document margins
\usepackage[left=0.75in,top=0.6in,right=0.75in,bottom=0.6in]{geometry}

% Set font face
\usepackage{fontspec}
\defaultfontfeatures{LetterSpace=-4}
\setmainfont{Iosevka Aile}

% Remove paragraph indentation
\usepackage[parfill]{parskip}

% Required for boldface (\bf and \bfseries) tabular columns
\usepackage{array}

% Required for ifthenelse statements
\usepackage{etoolbox}

\usepackage{fancyhdr}
\renewcommand{\headrulewidth}{0pt}
\fancyfoot[R]{Created using \href{https://github.com/waelbendhia/cv-gen}{CV-Gen}}
\fancyhf{}
\pagestyle{fancy}

% Flexible tables
\usepackage{tabularx}
\setlength{\tabcolsep}{6pt}
\renewcommand\tabularxcolumn[1]{m{#1}}

% For custom margins
\usepackage{changepage}

% To customize lists
\usepackage{enumitem}
% Remove left margin from lists, suppress vertical item separation and set top
% separation to a negative value to suppress a single parskip before lists
\setlist{leftmargin=*, label={}}

% For spcaed small caps
\usepackage{microtype}

\DeclareMicrotypeSet*{smallcapsi} {
	encoding = {OT1,T1,T2A,LY1,OT4,QX,T5,TS1,EU1,EU2},
	shape = {sc*,si,scit}
}

%--------------------------------------------------------------------------------
%                                   Constants                                   -
%--------------------------------------------------------------------------------

\def\namespaceskip{0.575em}
\def\afterheaderspace{1.1pt}
\def\beforesummaryspace{6pt}
\def\beforesectionheaderspace{3pt}
\def\beforesectionheaderhrulespace{2.85pt}
\def\sectionheaderhrlueheight{0.518pt}
\def\aftersectionheaderspace{0pt}

\def\aftersubsectionheaderspace{-18.25pt}

\def\summarymargin{6pt}
\def\sectionheadermargin{6pt}
\def\subsectionmargin{6pt}
\def\projectmargin{6pt}
\def\aftersubsectionspace{2.1pt}

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
	\textbf{\LARGE\textls[110]{\@name}}
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

\newcommand\printcontacts{
	\begin{small}
		{
			\setlength{\tabcolsep}{2pt}
			\begin{tabularx}{180pt}{
					>{\hsize=42pt\raggedleft\arraybackslash}X >{\hsize=120pt\raggedright\arraybackslash}X
				}
				% \textbf{󰇯 }    & \href{mailto:\@email}{\@email} \\
				% \textbf{ } & \href{https://linkedin.com/in/\@linkedin}{\@linkedin} \\
				% \textbf{ }   & \href{https://github.com/\@github}{\@github} \\
				\textbf{Email}    & \href{mailto:\@email}{\@email}                        \\
				\textbf{LinkedIn} & \href{https://linkedin.com/in/\@linkedin}{\@linkedin} \\
				\textbf{GitHub}   & \href{https://github.com/\@github}{\@github}          \\

			\end{tabularx}
		}
	\end{small}
}

\makeatother

\makeatletter
\newcommand\makeheader{
	\begin{center}
		\begin{tabularx}{1\textwidth}{
				>{\raggedright\arraybackslash}X >{\raggedleft\arraybackslash}X
			}
			\printname & \vspace*{-50pt}\printcontacts \\

		\end{tabularx}
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
	\vspace*{\beforesummaryspace}
	\begin{adjustwidth}{\summarymargin}{\summarymargin}
		}{
	\end{adjustwidth}
}
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
	\begin{color}{subheading}
		\begin{adjustwidth}{\sectionheadermargin}{\sectionheadermargin}
			\textbf{#1}
		\end{adjustwidth}
		\vspace*{\beforesectionheaderhrulespace}
		\hrule height \sectionheaderhrlueheight
	\end{color}
	\vspace*{\aftersectionheaderspace}
}{}
\makeatother

% Project body
\makeatletter
\newenvironment{project}[2]{
	\par
	\begin{samepage}
		\begin{color}{subheading}
			\begin{tabularx}{1\linewidth}{
					>{\raggedright\arraybackslash}X >{\raggedleft\arraybackslash}X
				}
				\textbf{#1} & #2 \\

			\end{tabularx}
		\end{color}
		\vspace*{\aftersubsectionheaderspace}
		\begin{adjustwidth}{\projectmargin}{\projectmargin}
			}{
		\end{adjustwidth}
	\end{samepage}
}
\makeatother

% Print a subsection
\makeatletter

\newenvironment{cvsubsection}[3]{
	\begin{color}{subheading}
		\begin{tabularx}{1\textwidth}{
				>{\raggedright\arraybackslash}X
				>{\centering\arraybackslash}X
				>{\raggedleft\arraybackslash}X
			}
			\textbf{#1} & \textbf{#2} & \textbf{#3} \\

		\end{tabularx}
	\end{color}
	% Add space according to the specidied number of lines
	\vspace*{\aftersubsectionheaderspace}
	\begin{adjustwidth}{\subsectionmargin}{\subsectionmargin}
		}{
	\end{adjustwidth}
	\vspace*{\aftersubsectionspace}
}
\makeatother

\sloppy
