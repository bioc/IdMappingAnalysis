%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  display.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{line.loess.Display}
\alias{line.loess.Display}
\alias{Display.line.loess}
\alias{line.loess.Display}
\alias{line.loess,Display-method}


 \title{Plot loess transformed data}

 \description{Plot loess transformed data.}

 \synopsis{line.loess.Display(static, x, y, ...)}
\usage{Display$line.loess(x, y, ...)}

 \arguments{
 \item{x}{The x coordinates of points to plot.}
 \item{y}{The y coordinates of points to be loess transformed for plotting.}
 \item{...}{Additional graphical parameters}
 }

 \examples{\dontrun{
 plot.new();
 plot.window(c(min(x),max(x)),c(min(y),max(y)));
 Display$line.loess(x,y);
 }}

 \seealso{For more information see \code{\link{Display}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
