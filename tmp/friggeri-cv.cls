\ProvidesClass{friggeri-cv}[2012/04/30 CV class]
\NeedsTeXFormat{LaTeX2e}

\DeclareOption{print}{\def\@cv@print{}}
\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{article}%
}
\ProcessOptions\relax
\LoadClass[a4paper,english,10pt]{article}


\usepackage{adforn}                % Other nice ornaments
\usepackage{pifont}
\usepackage{ulem}
\usepackage{graphicx}		% To include figure in the document
\graphicspath{{/}}


%%%%%%%%%%
% Colors %
%%%%%%%%%%

\RequirePackage{xcolor}

\definecolor{white}{RGB}{255,255,255}

\definecolor{darkgray}{HTML}{050505}
\definecolor{gray}{HTML}{050505}
\definecolor{lightgray}{HTML}{605F60}
\definecolor{lightgray3}{HTML}{e8e8e8}
\definecolor{lightgray2}{HTML}{979797}

%\definecolor{green}{HTML}{C2E15F}
%\definecolor{orange}{HTML}{FDA333}
%\definecolor{purple}{HTML}{D3A4F9}
%\definecolor{red}{HTML}{FB4485}
%\definecolor{blue}{HTML}{6CE0F1}


\definecolor{green}{HTML}{41A02A}
\definecolor{orange}{HTML}{F08C05}
\definecolor{purple}{HTML}{7C64A0}
\definecolor{purple2}{HTML}{6167E7}
\definecolor{red}{HTML}{D40000}
\definecolor{blue}{HTML}{056EE6}
\definecolor{brown}{HTML}{056EE6}
\definecolor{green2}{HTML}{44D39C}


\definecolor{light}{gray}{0.2}


\ifdefined\@cv@print
  \colorlet{green2}{lightgray2}
  \colorlet{green}{lightgray2}
  \colorlet{orange}{lightgray2}
  \colorlet{purple}{lightgray2}
  \colorlet{brown}{lightgray2}
  \colorlet{red}{lightgray2}
  \colorlet{blue}{lightgray2}
  \colorlet{fillheader}{lightgray3}
  \colorlet{header}{gray}
\else
  \colorlet{fillheader}{gray}
  \colorlet{header}{white}
\fi
\colorlet{textcolor}{gray}
\colorlet{headercolor}{gray}
\definecolor{fillheader}{HTML}{5A6167}

%%%%%%%%%
% Fonts %
%%%%%%%%%

\RequirePackage[quiet]{fontspec}
\RequirePackage[math-style=TeX,vargreek-shape=unicode]{unicode-math}

\newfontfamily\bodyfont[]{Helvetica Neue}
\newfontfamily\thinfont[]{Helvetica Neue UltraLight}
\newfontfamily\headingfont[]{Helvetica Neue Medium}

\newfontfamily\NHLight[
   UprightFont = HelveticaNeue-Thin,
   ItalicFont = HelveticaNeue-ThinItalic]{HelveticaNeue-Light}
   
\newfontfamily\NHMedium[
   UprightFont = HelveticaNeue-Medium]{HelveticaNeue-Medium}

\newcommand\textrmlf[1]{{\NHLight#1}}
\newcommand\textitlf[1]{{\NHLight\itshape#1}}

\newcommand\textrmmf[1]{{\NHMedium#1}}

\defaultfontfeatures{Mapping=tex-text}
\setmainfont[
    Mapping=tex-text, 
    Color=textcolor]{Helvetica Neue Light}

%\setmathfont{XITS Math}

%%%%%%%%%%
% Header %
%%%%%%%%%%

\RequirePackage{tikz}

\newcommand{\rolefont}{%
  \fontsize{14pt}{24pt}\selectfont%
  \thinfont%
  \color{white}%
}

\newcommand{\header}[3]{%
%  \begin{tikzpicture}[remember picture,overlay]
%    \node [rectangle, fill=fillheader, anchor=north, minimum width=\paperwidth, minimum height=4cm] (box) at (current page.north){};
%    \node [anchor=center] (name) at (box) {%
%      \fontsize{40pt}{32pt}\color{header}%
%      {\thinfont #1} {\bodyfont  #2}
%    };
%    \node [anchor=north] at (name.south) {%
%      \fontsize{14pt}{24pt}\color{header}%
%      \thinfont #3%
%    };
%  \end{tikzpicture}
%  \vspace{3.5cm}
%  \vspace{-2\parskip}
%}
   
    \vspace{-1.5cm}
    \rule{\linewidth}{2pt}\\

    \begingroup  
        \fontsize{25pt}{50pt}\selectfont
        {\thinfont #1} {\bodyfont #2}
    \endgroup    
    
    \begingroup  
        \fontsize{12pt}{24pt}\selectfont
        #3
    \endgroup 
    
\rule{\linewidth}{2pt}\\
}


\newcommand{\descr}[3]{%
  \parbox[t]{\linewidth}{%
    \textbf{#1}%
    \hfill%
    {\footnotesize\addfontfeature{Color=lightgray} #2}\\%
    #3\vspace{\parsep}%
  }\\}
  


%%%%%%%%%%%%%
% Structure %
%%%%%%%%%%%%%
\RequirePackage{parskip}

\newcounter{colorCounter}
\def\@sectioncolor#1#2#3{%
  {%
    \color{%
      \ifcase\value{colorCounter}%
        blue\or%
        red\or%
        orange\or%
        green\or%
        purple\or%
        brown\or%
        green2\or%
        red\or%
        orange\or%
        purple2\or%
        green\else%
        headercolor\fi%
    } #1#2#3%
  }%
  \stepcounter{colorCounter}%
}

\renewcommand{\section}[1]{
  \par\vspace{\parskip}
  {%
    \LARGE\headingfont\color{headercolor}%
    \@sectioncolor #1%
  }
  \par\vspace{\parskip}
}

\renewcommand{\subsection}[2]{
  \par\vspace{.5\parskip}%
  \Large\headingfont\color{headercolor} #2%
  \par\vspace{.25\parskip}%
}
%\pagestyle{plain}
\usepackage{lastpage}
\usepackage{fancyhdr}

\pagestyle{fancy}
\fancyhf{}
\rfoot{\thepage\ of \pageref{LastPage}} 
\rhead{Guillaume Lobet}
\renewcommand{\headrulewidth}{0pt}%
\fancyhfoffset[L]{-2pt}

\fancypagestyle{first}{
\fancyhf{}
\rfoot{\thepage\ of \pageref{LastPage}}}

%%%%%%%%%%%%%%%%%%%%
% List environment %
%%%%%%%%%%%%%%%%%%%%

\setlength{\tabcolsep}{0pt}
\newenvironment{entrylist}{%
  \begin{tabular*}{\textwidth}{@{\extracolsep{\fill}}ll}
}{%
  \end{tabular*}
}
\renewcommand{\bfseries}{\headingfont\color{headercolor}}
\newcommand{\entry}[4]{%
  #1&\parbox[t]{11cm}{%
    \textbf{#2}%
    \hfill%
    {\footnotesize\addfontfeature{Color=lightgray} #3}\\%
    #4\vspace{\parsep}%
  }\\}
  
  
%%%%%%%%%%%%%%
% Side block %
%%%%%%%%%%%%%%

\RequirePackage[absolute,overlay]{textpos}
\setlength{\TPHorizModule}{1.1cm}
%\setlength{\TPVertModule}{0.383cm}
\setlength{\TPVertModule}{0.6cm}
\newenvironment{aside}{%
  \let\oldsection\section
  \renewcommand{\section}[1]{
    \par\vspace{\baselineskip}{\Large\headingfont\color{headercolor} ##1}
  }
  \begin{textblock}{3.6}(1, 4.33)
  \begin{flushright}
  \obeycr
}{%
  \restorecr
  \end{flushright}
  \end{textblock}
  \let\section\oldsection
}

%%%%%%%%%%%%%%%%
% Bibliography %
%%%%%%%%%%%%%%%%

\RequirePackage[style=verbose, maxnames=99, sorting=ydnt, firstinits=true, backend=biber]{biblatex}

\DeclareFieldFormat[article]{title}{#1\par}
\DeclareFieldFormat[book]{title}{#1\par}
\DeclareFieldFormat[inproceedings]{title}{#1\par}
\DeclareFieldFormat[incollection]{title}{#1\par}
\DeclareFieldFormat[thesis]{title}{#1\par}
\DeclareFieldFormat[misc]{title}{#1\par}
\DeclareFieldFormat[report]{title}{#1\par}

\DeclareSourcemap{
    \maps[datatype=bibtex]{
        % Map the citations
        \map{
            \step[fieldsource=citations]
            \step[fieldset=usera,origfieldval]
        }
        % Map the readers
        \map{
            \step[fieldsource=altmetric]
            \step[fieldset=userb,origfieldval]
        }   
        % Map the inpact factors
        \map{
            \step[fieldsource=fcr]
            \step[fieldset=userc,origfieldval]
        }  
        % Map the quote
        \map{
            \step[fieldsource=quote]
            \step[fieldset=userd,origfieldval]
        }                     
    }
}

\DeclareBibliographyDriver{article}{%
  \textrmmf{\printfield{title}}%
  \newblock%
%  \printnames{author}%
  {%
    \small%
    \textrmlf{\printnames{author}}%
  }
  \par%
  \newblock%
  {%
    \small\addfontfeature{Color=lightgray}%
%    \usebibmacro{journal+issuetitle}%
%    \setunit{\space}%
%    \printfield{pages}%
%    \newunit%
%    \printlist{publisher}%
%    \setunit*{\addcomma\space}%
    \textrmlf{\printfield{year}%
    \newunit%
    \hspace{4pt}| \hspace{4pt}Citation(s):
    \printfield{usera}%
    \hspace{4pt}| \hspace{4pt}FCR:
    \printfield{userc}%
    \hspace{4pt}| \hspace{4pt}Altmetric score:
    \printfield{userb}}%
    }
  \par%
  \newblock%
  {%
     \small%
     \textrmlf{\printfield{url}}%
  } 
  \par%
  \newblock%
  {%
     \small%
     \textitlf{\printfield{userd}}%
  }  
  \par\vspace{0.8\baselineskip}
 }
 
 

\DeclareBibliographyDriver{incollection}{%
  \textrmmf{\printfield{title}}%
  \par%
  \newblock%
  {%
      \small
      \textrmlf{\printnames{author}}%
  }
  \par%
  \newblock%
  {%
    \small%
    \textrmlf{\printfield{booktitle}%
    \setunit*{\addcomma\space}%
    \printlist{publisher}%
    \setunit*{\addcomma\space}%
    \printfield{year}%
    \setunit{\addcomma\space}%
    \printlist{location}}%
    \newunit%
  }
  \par\vspace{0.8\baselineskip}
}


\DeclareBibliographyDriver{thesis}{%
  \textrmmf{\printfield{title}}%
  \newblock%
    {%
    \small%
    \textrmlf{\printnames{author}}%
  }
  \par%
  \newblock%
  {%
    \small%
    \textrmlf{\printlist{institution}%
    \setunit*{\addcomma\space}%
    \printfield{year}}%
  }
  \par%
  \newblock%
  {%
     \small%
     \textrmlf{\printfield{url}}%
  }  
  \par\vspace{0.3\baselineskip}
}



\DeclareBibliographyDriver{book}{%
  \textrmmf{\printfield{title}}%
  \newblock%
  {%
    \small%
    \textrmlf{\printnames{author}}%
  }  \par%
  \newblock%
  {%
    \small%
    \textrmlf{\printlist{publisher}%
    \setunit*{\addcomma\space}%
    \printfield{note}%
    \setunit*{\addcomma\space}%
    \printfield{year}%
    \setunit{\addcomma\space}%
    \printlist{location}}%
    \newunit%
  }
  \par\vspace{0.3\baselineskip}
}




\DeclareBibliographyDriver{inproceedings}{%
  \textrmmf{\printfield{title}}%
%  \newblock%
%  {%
%    \small%
%    \textrmlf{\printnames{author}}%
%  }  \par%
  \newblock%
  {%
    \small%
    \textitlf{\printfield{booktitle}}%
    \textrmlf{\setunit{\addcomma\space}%
    \printfield{pages}%
    \setunit{\addcomma\space}%
    \printlist{location}%
    \setunit{\addcomma\space}%
    \printfield{year}}%
  }
  \par%
  \newblock%
  {%
     \small%
     \textrmlf{\normalfont{\printfield{url}}}%
  } 
  \par\vspace{0.8\baselineskip}
}




%\DeclareNameFormat{author}{%
% \small\addfontfeature{Color=lightgray}%
%  \ifblank{#3}{}{\firstinit{#3}\adddot\space}#1%
%  \ifthenelse{\value{listcount}<\value{liststop}}
%    {\addcomma\space}
%    {}%
%}

\newcommand{\printbibsection}[2]{
  \begin{refsection}
    \nocite{*}
    \printbibliography[sorting=chronological, type={#1}, title={#2}, heading=subbibliography]
  \end{refsection}
}

\DeclareSortingScheme{chronological}{
  \sort[direction=descending]{\field{year}}
  \sort[direction=descending]{\field{month}}
}

\newcommand{\udensdot}[1]{%
    \tikz[baseline=(todotted.base)]{
        \node[inner sep=2pt,outer sep=0pt] (todotted) {#1};
        \draw[densely dotted] (todotted.south west) -- (todotted.south east);
    }%
}%

\newcommand{\udensdash}[1]{%
    \tikz[baseline=(todotted.base)]{
        \node[inner sep=1pt,outer sep=0pt] (todotted) {#1};
        \draw[densely dashed] (todotted.south west) -- (todotted.south east);
    }%
}%

\newcommand{\myname}[1]{\dashuline{Guillaume Lobet}}

%%%%%%%%%%%%%%%%
% Other tweaks %
%%%%%%%%%%%%%%%%

\RequirePackage[
    left=6.1cm,
    top=1cm,
    right=1.8cm,
    bottom=2cm,
    nohead
    ]
    {geometry}
    
\RequirePackage{hyperref}
\usepackage{fontawesome}


