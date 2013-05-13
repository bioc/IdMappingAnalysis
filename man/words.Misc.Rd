%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  misc.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{words.Misc}
\alias{words.Misc}
\alias{Misc.words}
\alias{words.Misc}
\alias{words,Misc-method}


 \title{Convert space delimited string to a vector of words}

 \description{
 Convert space delimited string to a vector of words.
 }

\usage{Misc$words(string, ...)}

 \arguments{
 \item{string}{Space delimited character string.}
 \item{...}{Not used}
 }

 \value{
 Character vector of words extracted from space delimited string
 }

 \examples{
 IDs<-Misc$words("1007_s_at 207169_x_at 208779_x_at 210749_x_at");
 }

 \seealso{For more information see \code{\link{Misc}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
