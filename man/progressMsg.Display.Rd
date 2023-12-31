%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  R/display.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{Display$progressMsg}
\alias{Display$progressMsg}
\alias{progressMsg.Display}
\alias{Display.progressMsg}
\alias{progressMsg,Display-method}

 \title{Display a progress message}

 \description{
 Display a message accompanied by the percentage of completion.
 }

 \usage{
## Static method (use this):
## Display$progressMsg(msg, i, total, ...)

## Don't use the below:
\method{progressMsg}{Display}(static, msg, i, total, ...)
}

 \arguments{
 \item{msg}{Message to be dysplayed.}
 \item{i}{Counter of the progress.}
 \item{total}{The maximum value for a counter corresponding to 100 percent completion.}
 \item{...}{Not used}
 }

 \examples{\dontrun{
 for (i in 1:100){
   Display$progressMsg("processing: ",i,100);
 # do something ...
 }
 }}

 \seealso{For more information see \code{\link{Display}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
