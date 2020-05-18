\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SVAchapter}[2020/03/29 SVA LaTeX chapter class]

%%A two sided book that has space for binding on the left side:
\LoadClass[a4paper, twoside]{report}

\RequirePackage[top=18mm, bottom=18mm, left=10mm, right=12mm, bindingoffset=15mm]{geometry}
\RequirePackage{wasysym}
%% \RequirePackage{fontspec}

\RequirePackage{color}
\RequirePackage{graphics}
\RequirePackage{graphicx}
\RequirePackage{threeparttable}

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
\RequirePackage{float}
\RequirePackage{multicol}
\RequirePackage{numprint}
\RequirePackage{url}

%%Separated the columns by a 6 mm
\setlength{\columnsep}{0.6cm}
