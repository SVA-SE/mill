\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SVAchapter}[2020/03/29 SVA LaTeX chapter class]

%%A two sided book that has space for binding on the left side:
\LoadClass[a4paper, twoside]{report}

\RequirePackage[top=18mm,
                bottom=18mm,
                left=10mm,
                right=12mm,
                bindingoffset=15mm]{geometry}
\RequirePackage{wasysym}
\RequirePackage{fancyhdr}

%% Set the fonts
\RequirePackage{fontspec}
\setmainfont{TeX Gyre Termes}
\setsansfont{Lato}

\newfontfamily\lightsf{Lato Light}[Scale = 0.9,
                                   BoldItalicFont = {Lato Light Italic},
                                   BoldFont = {Lato Light}]
\newfontfamily\covertitlefont{Lato Bold}

%% Sort out the spacing a style of the paragraph, section, subsection,
%% subsubsection and chapter titles

\usepackage[compact]{titlesec}

\titleformat{\part}[display]
    {\fontspec{Lato Bold}[Scale = 1.5]\centering}
    {\chaptertitlename\ \thechapter}{100pt}{\Huge}

\titleformat{\chapter}[display]
  {\fontspec{Lato}\huge}{\chaptertitlename\ \thechapter}{40pt}{\Huge\textbf}

\titlespacing*{\chapter}{0pt}{-20pt}{16pt}

\titleformat*{\section}{\fontspec{Lato Regular}\color{svared}\uppercase}
\titlespacing*{\section}{0pt}{12pt}{0pt}

\titleformat*{\subsection}{\fontspec{Lato Bold}}
\titlespacing*{\subsection}{0pt}{12pt}{0pt}

\titleformat*{\subsubsection}{\fontspec{Lato}\emph}
\titlespacing*{\subsubsection}{0pt}{12pt}{0pt}

\titleformat{\paragraph}[hang]
    {\fontspec{Lato}[Scale = 0.9]}
    {\theparagraph}{1em}{}
\titlespacing*{\paragraph}{0pt}{6pt}{0pt}

%%
\RequirePackage{color}
\RequirePackage{xcolor}
\RequirePackage{graphics}
\RequirePackage{graphicx}

%% Some colours
\definecolor{svagreen}{HTML}{85A727}
\definecolor{svadarkgreen}{HTML}{67841E}
\definecolor{svalightgreen}{HTML}{85A727}
\definecolor{svalightgreen2}{HTML}{B8DA5A}
\definecolor{svared}{HTML}{B21B1C}
\definecolor{lightgray}{gray}{0.80}
\definecolor{highlightgray}{gray}{0.90}
\definecolor{covertitle}{HTML}{3D3D3D}
\definecolor{covertitlemain}{HTML}{FFFFFF}

\RequirePackage[hidelinks,
  pdftitle={Surveillance of infectious diseases in animals
    and humans in Sweden 2019},
  pdfsubject={Surveillance of infectious diseases in animals
    and humans in Sweden 2019},
  pdfauthor={Editor: Karl Stahl, Department of Disease Control and
    Epidemiology, National Veterinary Institute (SVA),
    SE-751 89 Uppsala, Sweden.},
  pdfkeywords
  ={Sweden;
    Surveillance;
    Animal health;
    Zoonotic;
    One health}]{hyperref}

\RequirePackage{booktabs}
\RequirePackage{multirow}
\RequirePackage{tabularx}
\RequirePackage{threeparttable}
\RequirePackage{multicol}
\RequirePackage{numprint}
\RequirePackage{url}

%Font of urls
\urlstyle{rm}

%% Set up the font for captions
\RequirePackage[singlelinecheck=false]{caption}
%setfont for captions See the \setsansfont above from fontspec
\captionsetup{font = {sf, footnotesize}, format = hang}

%% formatting of tables:
%%
%% Define a new column type in tables so that you can avoind the
%% distributed text in a 'p' type column typically used for paragraph
%% text.
\RequirePackage{array}
\newcolumntype{L}[1]
    {>{\raggedright\let\newline\\\arraybackslash\hspace{0pt}}p{#1}}

%% Define the 'D' column type to align numbers by their decimal place
%% use like this: D{.}{.}{-1}
\RequirePackage{dcolumn}
% increase the default row spacing in tables
\renewcommand{\arraystretch}{1.0}

% Change the font size in all tables and the footnotes note the call
% to sf with is related to the definition of the sansfont
\RequirePackage{floatrow}
\DeclareFloatFont{footnotesize}{\footnotesize}
\floatsetup[table]{font= {sf, footnotesize}, capposition=top}

%%Separated the columns by a 6 mm
\setlength{\columnsep}{0.6cm}

\RequirePackage{tikz}
\newcommand{\includepicture}[4][H]{
\begin{figure}[#1]
  \begin{tikzpicture}
    \node [inner sep=0pt,outer sep=0pt,clip,rounded corners=0.0cm] (pict) at (0,0) {\includegraphics[width=\textwidth, trim = #4, clip]{#2}};
  \end{tikzpicture}
  \caption*{\scriptsize{#3}}
\end{figure}
}

%Line stretch (space between lines as a function of character height)
\renewcommand{\baselinestretch}{1.06}
\brokenpenalty = 10000
\raggedcolumns

\usepackage{adjustbox}

\newcolumntype{R}[2]{%
    >{\adjustbox{angle=#1,lap=\width-(#2)}\bgroup}%
    l%
    <{\egroup}%
}
\newcommand*\rot{\multicolumn{1}{R{75}{1em}}}
