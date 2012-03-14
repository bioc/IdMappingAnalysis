#######################################
# Data Analysis
#######################################

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
# Id Mapping related analysis
#####################################################

# ecdf plot
mapCounts<-ecdf.plot(jointIdMap,idMapNames=c("NetAffx_Q","NetAffx_F","DAVID_Q","DAVID_F","EnVision_Q"),
	new.plot=TRUE,file.copy=TRUE,copy.zoom=zoom,cex.lab=1.3,cex.main=1.4,bg=bg,verbose=verbose);

stats<-mapCounts$getStats(summary=TRUE,cutoff=3)
print(stats);
write.csv(stats,file="Probeset_summary.csv");


# fountain plots
db_pairs<-list(c("DAVID_Q","EnVision_Q"), c("NetAffx_Q","DAVID_Q"), c("NetAffx_Q","EnVision_Q"),
			c("NetAffx_F","DAVID_F"), c("NetAffx_Q","NetAffx_F"), c("DAVID_Q","DAVID_F"));

Display$create();
for (db_pair in db_pairs){
	jointIdMap$diffCounts.plot(db_pair,new.plot=FALSE,file.copy=TRUE,copy.zoom=zoom,
		adj=0.1,srt=0,sides=1,cex=1.3,cex.side=1,bg=bg,verbose=verbose);
	gc();
}

#####################################################
# Correlations related analysis
#####################################################
groups=list("NetAffx_Q"="AffQ","DAVID_Q"="DQ","EnVision_Q"="EnV");

# correlation density plot
jointUniquePairs$interactive.corr.plot(corr,groups=names(groups),
		subsetting=TRUE,new.plot=TRUE,file.copy=TRUE,copy.zoom=zoom,verbose=verbose,cex.lab=1.4,cex.legend=1.1,bg=bg);

# mixture model plot 
jointUniquePairs$interactive.mixture.plot(corr,groups=names(groups),
		subsetting=TRUE,new.plot=TRUE,file.copy=TRUE,copy.zoom=zoom,G=c(1:5),cex.lab=1.4,cex.legend=1.2,bg=bg,verbose=verbose);

# correlation boxplot
jointUniquePairs$interactive.corr.boxplot(corr,groups=groups,
	subsetting=TRUE,new.plot=TRUE,file.copy=TRUE,copy.zoom=zoom,srt=30,adj=0,cex=1,cex.lab=1.4,cex.axis=1.2,cex.main=1.4,bg=bg,verbose=verbose);

# mixture boxplot
jointUniquePairs$interactive.mixture.boxplot(corr,groups=groups,
		subsetting=TRUE,new.plot=TRUE,file.copy=TRUE,copy.zoom=zoom,srt=30,adj=0,cex=1,cex.lab=1.4,cex.axis=1.2,cex.main=1.4,G=c(1:5),plot.G=2,bg=bg);

# scatterplot 1
corrData$interactive.plot(input=list(c("P07355","1568126_at")),outcomePairs=examples$outcomeMap,proteinNames="ANXA2",
		new.plot=TRUE,file.copy=TRUE,copy.zoom=zoom,cols=c("green","red","darkblue"),cex=1.2,cex.main=1.2,font.main=4,cex.lab=1.2,bg=bg);

# scatterplot 2
corrData$interactive.plot(input=list(c("P07355","213503_x_at")),outcomePairs=examples$outcomeMap,proteinNames="ANXA2",
		new.plot=TRUE,file.copy=TRUE,copy.zoom=zoom,cols=c("green","red","darkblue"),cex=1.2,cex.main=1.2,font.main=4,cex.lab=1.2,bg=bg);


####################################################
# bootstrapping
####################################################

#create data structure containing the  bootstrapping results
#for all DB's in consideration(correlation and sd, takes ~30 sec to compute

bootstrap<-Bootstrap(corrData,Fisher=TRUE,verbose=verbose);

#sd vs corr scatterplot
bootstrap$plot(new.plot=TRUE,file.copy=TRUE,copy.zoom=zoom,bg=bg);

#create data structure containing the  bootstrapping results
#for specific DB's (correlation and sd, takes ~30 sec to compute)

bootstrap<-jointUniquePairs$getBootstrap(examples$msmsExperimentSet,examples$mrnaExperimentSet,
		groups=c("NetAffx_Q","DAVID_Q","DAVID_F","EnVision_Q"),verbose=verbose);

# sd vs corr scatterplot
bootstrap$plot(new.plot=TRUE,file.copy=TRUE,copy.zoom=zoom,bg=bg);






