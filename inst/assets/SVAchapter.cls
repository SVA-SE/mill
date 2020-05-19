\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SVAchapter}[2020/03/29 SVA LaTeX chapter class]

%%A two sided book that has space for binding on the left side:
\LoadClass[a4paper, twoside]{report}

\RequirePackage[top=18mm, bottom=18mm, left=10mm, right=12mm, bindingoffset=15mm]{geometry}
\RequirePackage{wasysym}

%% Set the fonts
\usepackage{fontspec}
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
  {\fontspec{Lato Bold}[Scale = 1.5]\centering}{\chaptertitlename\ \thechapter}{100pt}{\Huge}

\titleformat{\chapter}[display]
  {\fontspec{Lato}\huge}{\chaptertitlename\ \thechapter}{40pt}{\Huge\textbf}

\titlespacing*{\chapter}{0pt}{-20pt}{16pt}

\titleformat*{\section}{\fontspec{Lato Regular}\color{svared}\uppercase}
\titlespacing*{\section}{0pt}{12pt}{0pt}

\titleformat*{\subsection}{\fontspec{Lato Bold}}
\titlespacing*{\subsection}{0pt}{12pt}{0pt}

\titleformat*{\subsubsection}{\fontspec{Lato Italic}}
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
\RequirePackage{float}
\RequirePackage{multicol}
\RequirePackage{numprint}
\RequirePackage{url}

%%Separated the columns by a 6 mm
\setlength{\columnsep}{0.6cm}
