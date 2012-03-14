###########################################################################/**
# @set "class=JointIdMap"
# @RdocMethod diffCounts.plot
# 
# @title "Interactive wrapper for IdMapDiffCounts$plot"
#
# \description{
# Interactive wrapper of the IdMapDiffCounts.plot allowing to
# select the ID Map pairs encapsulated within the given JointIdMap object
# for a fountaine plot through the dialog.
# }
# 
# @synopsis
#
# \arguments{
# \item{idMapNames}{ either ID Maps pair names, 'menu' or 'loop'. If ID Map pairs, the functionality is similar to IdMapDiffCounts.plot
# If 'menu' or 'loop', user can select the pair of ID Maps interactively from the graphical selection dialog simultaneously controlling
# the plot destination (same or new graphic device) and wherther or not the plot should be saved in a file. If 'loop', the interactive
# drawing cycle repeats untill the user cancells the process.}
# \item{valRange}{ @numericl vector of length 3 where the first and second elements are minimum and maximum count values
# to be displayed on horizontal axis and the third element is a distance between horizontal axis tick marks.
# Default is c(-20,20,10).}
# \item{reverse}{ logical indicating the plot orientation (top to bottom or bottom to top). Default is @FALSE.}
# \item{pairLabels}{ optional decorated names for ID Map pair in consideration. If @NULL, the original ID Map names
# are used. Default is@ NULL.}
# \item{cols}{ colors corresponding to the <A-A*B, A*B, B-A*B> events from which the plot is composed.}
# \item{sides}{ (1 or 2): how compound events labels and counts are placed on plot. If sides=1 both event labels and counts
# are placed on one (left) side. If sides=2 then labels are placed on left side and counts on the right.}
# \item{cex}{ plot title font size.}
# \item{cex.side}{ compound events labels font size.}
# \item{srt}{ compound events labels orientation.}
# \item{adj}{ compound events labels position adjustment (0 - 1), where 0/1 corresponds 
# to the minimum/maximum labels position shift inwards the plot.}
# \item{new.plot}{ logical indicating if the new graphic device should be created for plot.
# In interactive mode can be selected graphically. Default is @FALSE.}
# \item{file.copy}{@logical or a @character string indicating if the plot should be saved to a file.
# If @character string, it's content used as a file name and if @TRUE, the default file named is used
# If @logical, in interactive mode can be toggled graphically. Default is @FALSE.}
# \item{copy.zoom}{ Zoom factor used when saving plot in a file.  Default is 1.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
#
# #non-interactive fountain plot of differences
# #between two DBs encapsulated within the JointIdMap object
# jointIdMap$diffCounts.plot(idMapNames=c("NetAffx_F","DAVID_Q"));
# \dontrun{
# #interactive plot allowing to choose the DB pair to be plotted
# jointIdMap$diffCounts.plot();
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################

setMethodS3("diffCounts.plot","JointIdMap",function(this,idMapNames="loop",valRange=c(-20,20,10),reverse=FALSE, pairLabels=NULL,
						new.plot=FALSE,file.copy=FALSE,copy.zoom=1,
						cols=c("red","blue","green"),sides=2,cex=1,cex.side=0.75*cex,srt=0,adj=0.5,verbose=TRUE,...){

  if (idMapNames[1]=="menu")
	mode<-"menu"
  else if (idMapNames[1]=="loop")
	mode<-"loop"
  else
	mode<-"items";

  stat<-list();
  cache<-list();

  mapNames<-getMapNames(this);	
  repeat{
	file.copy.name=ifelse(file.copy,"Turn Save Plot off","Turn Save Plot on");

	if(mode=="menu" || mode=="loop") {
		items<-rselect.list(c(file.copy.name,"New plot","Swap sides"," ",mapNames), multiple = TRUE,
			 title = "Fountain plot: choose DB Pairs");
		if(length(items)==0)
			break;
			#return(invisible(stat));

		file.copy<-ifelse(file.copy.name %in% items,!file.copy,file.copy);
		new.plot<-("New plot" %in% items);
		swap<-("Swap sides" %in% items);

		items<-items[!(items %in% c(" ","Swap sides","New plot",file.copy.name))];
		
		if (length(items)<2){
			warning("Two items should be selected");
		} else {
			if(swap){
				idMapNames<-c(items[2],items[1]);
			} else {
				idMapNames<-c(items[1],items[2]);
			}
		}
	}
	
	if (length(idMapNames)>1){
		
		diffName<-paste(idMapNames[1],idMapNames[2],sep="_vs_");

		diffCounts<-cache[[diffName]];
		if(is.null(diffCounts)){
			diff<-getDiff(this,idMapNames[1],idMapNames[2],verbose);
			diffCounts<-IdMapDiffCounts(diff,verbose);
			if(mode=="loop")
				cache[[diffName]]<-diffCounts;	
		}	

		#plot comparisons
		if(new.plot)
			Display$create();
	
		if("bg" %in% names(list(...)))
			par(bg=list(...)$bg);	

		plot(diffCounts,valRange=valRange,reverse=reverse, pairLabels=pairLabels,
						cols=cols, sides=sides,cex=cex,cex.side=cex.side,srt=srt,adj=adj,...);
		if(is.character(file.copy) || file.copy){
			if(is.logical(file.copy)){
				filename<-paste(diffName,"diff","png",sep=".");
			} else {
				filename<-file.copy;
			}
			Display$copy(filename,zoom=copy.zoom,
				plotFun=diffCounts$plot,plotArgs=c(list(reverse=reverse, pairLabels=pairLabels,
				cols=cols,sides=sides,cex=cex,cex.side=cex.side,srt=srt,adj=adj,par.zoom=copy.zoom),list(...)));
		}

		#summary
		stat[[diffName]]<-diffCounts;
	}
	gc();
	if(mode!="loop"){break};
  }

  if(mode!="loop"){
	if(length(stat)>0){	
		stat<-stat[[1]];
	} else {
		stat<-NULL;
	}
  }
	
  invisible(stat);
})



###########################################################################/**
# @RdocMethod ecdf.plot
# 
# @title "Interactive wrapper for IdMapCounts$plot"
#
# \description{
# Interactive wrapper of the IdMapDiff$plot allowing to
# select the ID Map encapsulated within the given JointIdMap object
# for a ecdf plot through the dialog.
# }
# 
# @synopsis
#
# \arguments{
# \item{jointIdMap}{ JointIdMap object from which ID Map counts information is extracted.}
# \item{idMapNames}{ either ID Map names, 'menu' or 'loop'. If ID Map names,
# the functionality is similar to IdMapCounts.plot If 'menu' or 'loop', 
# user can select the ID Maps interactively from the graphical selection 
# dialog simultaneously controlling the plot destination (same or new graphic device)
# and wherther or not the plot should be saved in a file. If 'loop', the interactive
# drawing cycle repeats untill the user cancells the process. Default is names of all
# ID Maps within the jointIdMap object.}
# \item{complement}{ @logical indicating wherether the complementary ecdf should be plotted
# the default is @TRUE.}
# \item{log}{ @logical indicating if log scale should be used. The default is @TRUE.}
# \item{new.plot}{ @logical indicating if the new graphic device should be created for plot.
# In interactive mode can be selected graphically. Default is @FALSE.}
# \item{file.copy}{@logical or a @character string indicating if the plot should be saved to a file.
# If @character string, it's content used as a file name and if @TRUE, the default file named is used.
# In interactive mode can be toggled graphically. Default is @FALSE.}
# \item{copy.zoom}{ Zoom factor used when saving plot in a file. Default is 1.}
# \item{verbose}{if @TRUE enables diagnostic messages. Default is @FALSE.}
# \item{...}{Not used}
# }
#
# \examples{
# jointIdMap<-JointIdMap(examples$identDfList);
#
# #non-interactive ecdf plot
# #of 3 DBs DBs encapsulated within the JointIdMap object
# jointIdMap$ecdf.plot(idMapNames=c("NetAffx_F","DAVID_Q","EnVision_Q"));
# \dontrun{
# #interactive plot allowing to choose the DBs to be plotted
# jointIdMap$ecdf.plot();
# }}
#
# \seealso{@seeclass}
# \author{Alex Lisovich, Roger Day}
#*/###########################################################################


setMethodS3("ecdf.plot","JointIdMap",function(this,idMapNames="loop",
			complement=TRUE,log=TRUE,new.plot=FALSE,file.copy=FALSE,copy.zoom=1,verbose=TRUE,...){

  if (idMapNames[1]=="menu")
	mode<-"menu"
  else if (idMapNames[1]=="loop")
	mode<-"loop"
  else {		
	mode<-"items";
  }
 
  stat<-list();
  cache<-list();

  mapNames<-getMapNames(this);	
  repeat{
	file.copy.name=ifelse(file.copy,"Turn Save Plot off","Turn Save Plot on");

	if(mode=="menu" || mode=="loop") {
		items<-rselect.list(c(file.copy.name,"New plot"," ",mapNames), multiple = TRUE,
			 title = "Ecdf plot: choose DBs");
		if(length(items)==0){
			break;
			#return(invisible(stat));
		}

		new.plot<-("New plot" %in% items);
		file.copy<-ifelse(file.copy.name %in% items,!file.copy,file.copy);

		items<-items[!(items %in% c(" ","New plot",file.copy.name))];
		idMapNames<-items;
		
	}
	
	if (length(idMapNames)>0){
		
		ecdfName<-paste(idMapNames,collapse="-");

		cnts<-cache[[ecdfName]];
		if(is.null(cnts)){
			if (is.list(idMapNames)){
				dbNames<-names(idMapNames);
			} else {
				dbNames<-idMapNames;
			}

			cnts<-getCounts(this,dbNames,verbose=verbose);

			if(mode=="loop")
				cache[[ecdfName]]<-cnts;	
		}	

		#plot comparisons
		if(new.plot)
			Display$create();
	
		if("bg" %in% names(list(...)))
			par(bg=list(...)$bg);	

		plot(cnts,idMapNames=idMapNames,complement=complement,log=log,...);

		if(is.character(file.copy) || file.copy){
			if(is.logical(file.copy)){
				filename<-paste(ecdfName,"ecdf","png",sep=".");
			} else {
				filename<-file.copy;
			}
			Display$copy(filename,copy.zoom,plotFun=cnts$plot,
				plotArgs=c(list(idMapNames=idMapNames,complement=complement,log=log,par.zoom=copy.zoom),list(...)));
		}


		#cach IdMapCounts
		stat[[ecdfName]]<-cnts;
	}
	gc();
	if(mode!="loop"){break};
  }

  if(mode!="loop"){
	if(length(stat)>0){	
		stat<-stat[[1]];
	} else {
		stat<-NULL;
	}
  }
	
  invisible(stat);
})


