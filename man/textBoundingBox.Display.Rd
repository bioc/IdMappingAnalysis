%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  display.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{textBoundingBox.Display}
\alias{textBoundingBox.Display}
\alias{Display.textBoundingBox}
\alias{textBoundingBox.Display}
\alias{textBoundingBox,Display-method}


 \title{Determine the size of the text bounding box}

 \description{
 Determine the size of the text bounding box
 taking into account the text angle (srt).
 }

 \note{
 The function should be called only on already created
 graphical device, otherwise an error will be thrown.
 }

 \synopsis{textBoundingBox.Display(static, stext, units="user", cex=1, srt=0, get.biggest=TRUE, ...)}
\usage{Display$textBoundingBox(stext, units="user", cex=1, srt=0, get.biggest=TRUE, ...)}

 \arguments{
 \item{stext}{The text which width and height to be determined.}
 \item{units}{If 'user' (default) the aspect ratio is calculated based on 'usr' and 'pin'.
 graphics parameters or kept equal to 1 otherwise.}
 \item{cex}{Text font size. Default is 1.}
 \item{srt}{Text angle.Default is 0.}
 \item{get.biggest}{If \code{\link[base:logical]{TRUE}} (default), returns the biggest bounding box
 in case stest contains multiple strings.}
 \item{...}{Not used}
 }

 \value{
 The \code{\link[base]{data.frame}} with two columns containing the width and height of the bounding box(es).
 }

 \examples{\dontrun{
 bounding.box<-Display$textBoundingBox("text",cex=2,srt=30);
 }}

 \seealso{For more information see \code{\link{Display}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
