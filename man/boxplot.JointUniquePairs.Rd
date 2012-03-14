%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  jointUniquePairs.plots.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{boxplot.JointUniquePairs}
\alias{boxplot.JointUniquePairs}
\alias{JointUniquePairs.boxplot}
\alias{boxplot.JointUniquePairs}
\alias{boxplot,JointUniquePairs-method}


 \title{ Draw a basic boxplot based on a given JointUniquePairs object and external data}

 \description{
 Draw a basic boxplot (without title and x/y labs) based on a given set of matches
 from the JointUniquePairs object and auxiliary data (response.data) from another data type.
 Reused for example within the corrboxplot() and mixture.boxplot() by providing
 specific  response.data to this function and adding the title and x/y labels after the basic plot is complete
 }

 \usage{\method{boxplot}{JointUniquePairs}(x, response.data, args=NULL, showNone=NA, multiline=FALSE, plot=TRUE, truncate.labels=TRUE, group.gap=0.3, cex=1, cex.main=1.2, cex.lab=1, cex.axis=1, srt=0, adj=0, par.zoom=1, addPoints=TRUE, col.points="lightblue", allAgree="All Agree", ...)}

 \arguments{
 \item{response.data}{The \code{\link[base]{numeric}} data vector on which the distribution for each particular match group
 is computed on the fly during the plot.}
 \item{args}{Optional \code{\link[base]{list}} of ID Map names within the UniquePairsMatch object to be plotted
 allowing to plot a subset of match groups. Default is \code{\link[base]{NULL}}.}
 \item{showNone}{If not \code{\link[base]{NA}}, includes a plot of subset of values not included into
 any other match group using showNone label. The default is \code{\link[base]{NA}}.}
 \item{multiline}{If \code{\link[base:logical]{TRUE}} the compaund expression x axis labels plotted in multiple lines,
 i.e. 'A & B' is plotted as 'A &<line break> B'. Default is \code{\link[base:logical]{FALSE}}}
 \item{plot}{if \code{\link[base:logical]{TRUE}} (default) then a boxplot is produced.
 If not, the summaries which the boxplots are based on are returned.}
 \item{group.gap}{Extra gap between boxplot groups. Default is 0.3.}
 \item{cex}{Plot symbols font size. Default is 1.}
 \item{cex.main}{Main title font size. Default is 1.2.}
 \item{cex.lab}{X and Y titles font size. Default is 1.}
 \item{cex.axis}{X and Y axis labels font size. Default is 1.}
 \item{srt}{Match group label orientation (see par('srt')). The default is 0.}
 \item{adj}{\code{\link[base]{numeric}} in a range (-1..1) determening how close to the x-axis
 the group labels are, negative being the closest and positive the furthest. The default is 0.}
 \item{par.zoom}{Graphics parameters zoom factor. Scales the graphical parameters like cex, lwd, mai etc.}
 \item{addPoints}{If \code{\link[base:logical]{TRUE}} (default), the points representing the data density distribution
 of a particular compaund group are plotted along with a standard box.}
 \item{col.points}{The color of the data points plotted along with a particular box.}
 \item{allAgree}{The text plotted for a compaund group corresponding
 to intersection of all match groups.}
 \item{...}{Additional graphical parameters}
 }

 \value{
 The data structure with following components:
 \item{boxdata}{Same as for boxplot in graphics package.}
 \item{response.grouped}{Input data grouped by matches.}
 }

 \examples{
 args=list(NetAffx_Q="Affy_Q",DAVID_F="D_F",EnVision_Q="EnQ");
 data<-examples$jointUniquePairs$boxplot(examples$corr$getData(),args,srt=30,allAgree="All");
 data$boxdata;
 names(data$response.grouped);
 }

 \seealso{For more information see \code{\link{JointUniquePairs}}.}
 \author{Alex Lisovich, Roger Day}
\keyword{internal}
\keyword{methods}
