#####################################################
# Interactive data exploration
#####################################################

library(IdMappingAnalysis);

#set verbosity level
verbose<-TRUE;

#set the plot background 
bg<-"white";

#set zoom factor to copy plot into a file
zoom<-2;

#####################################################
# load and preprocess sample ID Map collection, MS/MS and mRNA experiment data
#####################################################

data(examples);

#restrict MS/MS experiment data by rows for which 
#the avg. of counts for all samples (experiments) is greater than 0.52
fltExperimentSet<-DataFilter$do.apply(examples$msmsExperimentSet,
   byRows=TRUE,filterFun=DataFilter$minAvgCountConstraint,filtParams=0.52,verbose=verbose);
fltExperimentSet<-DataFilter$removeNASeries(fltExperimentSet,byRows=TRUE,verbose=verbose);

#log10-transform MRNA experiment set
examples$mrnaExperimentSet[,-1]<-log10(examples$mrnaExperimentSet[,-1]);

##########################################################################################################################
#### At this moment we have a collection of unrelated ID Maps which will be aligned during farther processing           ##
##########################################################################################################################


#####################################################
# define the primary and secondary IDs to work with
#####################################################

primaryIDs_mapping<-IdMapBase$primaryIDs(examples$msmsExperimentSet);
primaryIDs_corr<-IdMapBase$primaryIDs(fltExperimentSet);
secondaryIDs<-IdMapBase$primaryIDs(examples$mrnaExperimentSet);

############################################################################################################################
# create two joint ID Map objects which encapsulate the wholeID Map set aligned by the different primaryID sets            ##
############################################################################################################################

jointIdMap<-JointIdMap(examples$identDfList,primaryIDs_mapping,verbose=verbose);
jointIdMap_corr<-JointIdMap(examples$identDfList,primaryIDs_corr,verbose=verbose);


#####################################################
# correlation related processing
#####################################################

#create the unique pairs object containing only the secondaryIDs
#from the union of all ID maps  within the joint ID map object
uniquePairs<-as.UniquePairs(getUnionIdMap(jointIdMap_corr,verbose=verbose),secondaryIDs); 


#create pairs match object from unique pairs and joint ID map object
jointUniquePairs<-JointUniquePairs(uniquePairs,
		getIdMapList(jointIdMap_corr,verbose=verbose),verbose=verbose);


#create the aligned data structure suitable for fast correlations
#from the unique pairs set and two experiment sets
corrData<-CorrData(uniquePairs,examples$msmsExperimentSet,examples$mrnaExperimentSet,verbose=verbose);

#create correlation data object (perform correlations)
corr<-Corr(corrData,method="pearson",verbose=verbose);

#create the mixture object
mixture<-Mixture(corr,G=c(1:5),verbose=verbose);


#####################################################
# Id Mapping related interactive analysis
#####################################################

mapCounts<-jointIdMap$ecdf.plot("loop",copy.zoom=zoom,verbose=verbose,cex.lab=1.2)[[1]];
mapCounts$getStats(summary=FALSE,cutoff=3);
mapCounts$getStats(summary=TRUE,cutoff=3);

diffCounts<-jointIdMap$diffCounts.plot("loop",copy.zoom=zoom,
	adj=0.1,sides=1,verbose=verbose,cex=1.2)[[1]];
diffCounts$summary(verbose);


#####################################################
# correlations related interactive analysis
#####################################################

# interactive correlation density plot
jointUniquePairs$interactive.corr.plot(corr,copy.zoom=zoom,verbose=verbose,cex.lab=1.4);

# interactive mixture plot 
jointUniquePairs$interactive.mixture.plot(corr,copy.zoom=zoom,G=c(1:5),verbose=verbose);

#interactie correlation boxplot
jointUniquePairs$interactive.corr.boxplot(corr,copy.zoom=zoom,multiline=TRUE,
	srt=45,adj=0,cex=1,cex.lab=0.8,cex.axis=1,cex.main=1.2,bg=bg,verbose=verbose);


#interactive mixture boxplot
jointUniquePairs$interactive.mixture.boxplot(corr,G=c(1:5),plot.G=2,copy.zoom=zoom,multiline=TRUE,
	srt=45,adj=0,cex=1,cex.lab=0.8,cex.axis=1,cex.main=1.2,bg=bg,verbose=verbose);


####scatterplots
# general with protein choice and outcome
corrData$interactive.plot(input=c("P07355", "P07384", "P09382"),outcomePairs=examples$outcomeMap,
	cols=c("green","red","darkblue"),copy.zoom=zoom);

#  general with protein choice and no outcome (displays clusters);
corrData$interactive.plot(input=c("P07355", "P07384", "P09382"),copy.zoom=zoom,);

#outcome for P07355
corrData$interactive.plot(input=c("P07355"),outcomePairs=examples$outcomeMap,proteinNames="ANXA2",
	cols=c("green","red","darkblue"),copy.zoom=zoom);

#clusters for P07355
corrData$interactive.plot(input=c("P07355"),proteinNames="ANXA2",
	cols=c("green","red","darkblue"),copy.zoom=zoom);

####################################################
# bootstrapping 
# non-interactive due to the long computation time
# but allows to choose the DB's on which it is performed
####################################################

#create data structure containing the  bootstrapping results
#for specific DB's
bootstrap<-jointUniquePairs$getBootstrap(examples$msmsExperimentSet,examples$mrnaExperimentSet,
		groups=c("NetAffx_Q","DAVID_Q","DAVID_F","EnVision_Q"),verbose=verbose);

# sd vs corr scatterplot
bootstrap$plot(new.plot=TRUE,file.copy=TRUE,copy.zoom=zoom,bg=bg);







