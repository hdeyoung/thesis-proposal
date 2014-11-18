\RequirePackage{expl3}
\ProvidesClass{hdeyoung-proposal}
% {18-03-2014}{v0.1}{Proposal Document Class}

\LoadClass{article}

\PassOptionsToPackage{
  nochapters,
  pdfspacing,
  listings,
  floatperchapter
}{classicthesis}
\RequirePackage{classicthesis}



% %% TEXT FONTS
% \PassOptionsToPackage{T1}{fontenc}
% \RequirePackage{fontenc}

% \RequirePackage{lmodern} % For Latin Modern tt font
% % \RequirePackage[varqu,varl,var0]{zi4}

% \PassOptionsToPackage{
%   semibold,
%   tt=false,
%   lining % No oldstyle figures in math
% }{libertine}
% \RequirePackage{libertine}

% \RequirePackage{microtype}


% %% MATH
% \RequirePackage{amsmath}
% \RequirePackage{amssymb}

% \PassOptionsToPackage{amsthm,thmmarks}{ntheorem}
% \RequirePackage{ntheorem}

% \PassOptionsToPackage{libertine,bigdelims}{newtxmath}
% \RequirePackage{newtxmath}
% \useosf

% \theoremstyle{plain}
% \newtheorem{theorem}{Theorem}
% \newtheorem{lemma}{Lemma}
% \newtheorem{corollary}{Corollary}
% \newtheorem*{falseclaim}{False claim}
% \theoremstyle{definition}
% \theoremsymbol{\ensuremath{\Box}}
% \newtheorem{definition}{Definition}


% %% SECTIONS

% % \RequirePackage{titlesec}

% % \PassOptionsToPackage{dvipsnames}{xcolor}
% % \RequirePackage{xcolor}
% % \RequirePackage{textcase}

% % \titleformat{\section}[hang]{\Large\bfseries\scshape}{\thesection}{2ex}{}[]
% % \titleformat{\subsection}[hang]{\large\bfseries\scshape}{\thesubsection}{2ex}{}[]
% % \titleformat{\subsubsection}[hang]{\normalsize\bfseries\scshape}{\thesubsubsection}{2ex}{}[]
% % \titleformat{\paragraph}[runin]{\normalsize\bfseries}{\theparagraph}{0ex}{}[.]


% %% BIBLIOGRAPHY and REFERENCES

% \PassOptionsToPackage{
%   backend=biber,
%   style=authoryear-comp,
%   maxcitenames=2,
%   uniquelist=false
% }{biblatex}
% \RequirePackage{biblatex}

% \renewcommand*{\subtitlepunct}{:\ }

% \PassOptionsToPackage{capitalize}{cleveref}
% \RequirePackage{cleveref}




% %% MISCELLANEOUS
% \RequirePackage{csquotes}

% \PassOptionsToPackage{inline}{enumitem}
% \RequirePackage{enumitem}

% \RequirePackage{acro}

% \RequirePackage{booktabs}
% \RequirePackage{caption}[2013/01/01] % needs v3.3 or newer

% % \RequirePackage{fixme}
% % \fxsetup{inlineface=\color{blue}, nomargin, inline, status=draft}
% % \makeatletter
% % \renewcommand*\FXLayoutInline[3]{%
% %   {\@fxuseface{inline}#2}}
% % \makeatother
% % \RequirePackage{soul}

% % \NewDocumentCommand{\wc}{m o}{%
% %   #1\fxnote{\IfValueT{#2}{\ #2}\textsuperscript{?}}%
% % }



% % \NewDocumentCommand{\vocab}{m}{\textit{#1}}
% % \NewDocumentCommand{\eg}{}{e.g.}