
###########################################################################/**
# @RdocClass Bootstrap
# \encoding{latin1}
#
# @title "The Bootstrap class"
#
# \description{
#  @classhierarchy
# The Bootstrap object encapsulates a data frame containing 
# the unique pairs in the first two columns and the correlation results, sd and bias 
# obtained from the bootstrapping procedure in the next 3 columns
# During the object creation, the bootstrapping procedure is applyied to each row
# of the experiment set pairs from the CorrData object optionally applying the Fisher transform
# to the correlation data.
# }
# 
# @synopsis
#
# \arguments{
# \item{corrData}{CorrData object on which the correlation related bootstrapping is performed.}
# \item{Fisher}{If @TRUE, the Fisher transform of data is performed during bootstrapping. Default is @FALSE.}
# \item{R}{The number of bootstrap replicates. Default is 200.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used.}
# }
#
# \value{
# A Bootstrap object encapsultating the @data.frame with following columns:
# \item{column 1}{the first component (primary IDs) of unique pairs. The column name corresponds to the primary key
# of a source ID Map}
# \item{column 2}{the second component (secondary IDs) of unique pairs. The column name corresponds to the secondary key
# of a source ID Map}
# \item{'corr' column}{contains the correlation values obtained from bootstrapping}
# \item{'sd' column}{contains the correlation sd values obtained from bootstrapping}
# \item{'bias' column}{contains the correlation bias values obtained from bootstrapping}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \examples{
# bootstrap<-Bootstrap(examples$corrData,R=20,verbose=TRUE);
# class(bootstrap);
# bootstrap[1:10,];
# }
#
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setConstructorS3("Bootstrap",function(corrData=NULL,Fisher=FALSE,R=200,verbose=FALSE,...){
	if(is.null(corrData)){
		return(extend(IdMapBase(),"Bootstrap",.nuPoints=NULL));
	}

	FUN = function(data, indices){
			cor(data[indices,1],data[indices,2]);
	}	
	if(Fisher){
		statf = function(data, indices) DataFilter$fisherTransform(FUN(data, indices))
	} else {
		statf = FUN;
	}


	if (verbose)
		cat("Performing bootstrap R=",R,"on correlations...\n");

	analysisStructure<-corrData$.data;
	
	rows<-dim(analysisStructure[[1]])[1];
	cols<-dim(analysisStructure[[1]])[2];
	datacols<-c(2:cols);

	X<-data.matrix(analysisStructure[[1]][,datacols]);
	Y<-data.matrix(analysisStructure[[2]][,datacols]);

	res<-as.data.frame(array(NA,dim=c(rows,5)));
	for (i in 1:rows) {
		if (verbose)
			Display$progressMsg("processed:",i,rows);
		dat<-data.matrix(cbind(X[i,],Y[i,]));
		bootOut<-boot::boot(dat,statistic=FUN,R=R);
		boots = with(bootOut, 
			cbind(t0,
     				sqrt(apply(t, 2L,	function(t.st) { var(t.st[!is.na(t.st)])})),					
				apply(t, 2L, mean, na.rm = TRUE) - t0
			)
		)
		res[i,c(3:5)]<-boots;
	}
	if (verbose)
		cat("\n");
				
	res[,1]<-analysisStructure[[1]][,1];
	res[,2]<-analysisStructure[[2]][,1];

	colnames(res)<-c(names(analysisStructure),"corr","sd","bias");
	res<-as.data.frame(res,stringsAsFactors=FALSE);
	
	extend(IdMapBase(res,secondaryKey=colnames(res)[2]),"Bootstrap",
		.nuPoints=ncol(analysisStructure[[2]])-1
	);
})


#' @export plot.Bootstrap
###########################################################################/**
# @RdocMethod plot
# 
# @title "Scatterplot of bootstrapped results: sd vs correlation "
#
# \description{
# Scatterplot of the sd vs correlation values obtained from the pre-computed Bootstrap object
# along with two fitted lines: the first (solid blue) for a bootstrap smooth
# and a second (dashed red) for a normal theory formula
# }
# 
# @synopsis
#
# \arguments{
# \item{new.plot}{@logical indicating if the new graphic device should be created for plot. Default is @FALSE.}
# \item{file.copy}{@logical or a @character string indicating if the plot should be saved to a file.
# If @character string, it's content used as a file name and if @TRUE, the default file named is formed and used. Default is @FALSE.}
# \item{cex.main}{Main title font size. Default is 1.2.}
# \item{cex.lab}{X and Y titles font size. Default is 1.}
# \item{cex.axis}{X and Y axis labels font size. Default is 1.}
# \item{cex.legend}{plot legend font size. Default is par('cex').}
# \item{copy.zoom}{ Zoom factor used when saving plot in a file. Default is 1.}
# \item{...}{Additional graphic parameters}
# }
#
# \examples{
# examples$bootstrap$plot();
# }
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("plot","Bootstrap",function(x,new.plot=FALSE,file.copy=FALSE,copy.zoom=1,
	cex.main=1.2,cex.lab=1,cex.axis=1,cex.legend=1,...){

  BootstrapCorr.plot.internal<-function(bootstrap,cex.main,cex.lab,cex.axis,cex.legend,par.zoom=1,...){



	op<-Display$zoom.pars(par.zoom);

	if("bg" %in% names(list(...)))
		par(bg=list(...)$bg);
	

	nuPoints<-x$.nuPoints;

	with(x$.df,{
		plot(corr, sd, col="gray",cex.lab=cex.lab,cex.lab=cex.lab,cex.axis=cex.axis,...);
		Display$line.loess(corr, sd, col="blue", lwd=3*par("lwd"));
		Display$line.loess(corr, ((nuPoints-3)/(1-corr^2))^(-1/2), col="red", lwd=3*par("lwd"), lty=2 );
		axis(1,labels=FALSE,lwd=par("lwd"),cex.axis=cex.axis,...);
		axis(2,labels=FALSE,lwd=par("lwd"),cex.axis=cex.axis,...);
	})

	par(bg="transparent");

    	#legend drawing
	legend(x="topright",inset=0.05,
		legend=c("bootstrap smooth","normal theory"),
		col=c("blue","red"),lty=c(1,2),lwd=2*par("lwd"),cex=cex.legend);
	par(op);
  } # BootstrapCorr.plot.internal

  if(new.plot)
	Display$create();

  BootstrapCorr.plot.internal(x,cex.main,cex.lab,cex.axis,cex.legend,...);

  if(is.character(file.copy) || file.copy){
		if(is.logical(file.copy))
			filename<-"bootstrap.png"
		else
			filename<-file.copy;
		Display$copy(filename,zoom=copy.zoom,plotFun=BootstrapCorr.plot.internal,
		plotArgs=c(list(x,cex.main,cex.lab,cex.axis,cex.legend,par.zoom=copy.zoom),list(...)));
  }
},createGeneric=FALSE)

	

 
