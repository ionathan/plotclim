##################################################################                
# This is a set of functions for plotting AgMIP climate projections.
# 
# Author: Ioannis N. Athanasiadis (ioannis@athanasiadis.info)
#					Democritus Univ. of Thrace, http://eco.logismi.co                          
# Since: July 1, 2013                            
#
# Version: 0.9 ("Uganda")
#                                                                              
# This software is distributed under the GPL licence (GPL>v3).      
##################################################################
#   How to use:
#  * Load this file
# 	source("plotclim.R")
#  * Change directory where your AgMIP files are. ie:
#  	setwd("/Users/ioannis/Virtual Share/climate.tools.agmip/ETAG");
#  * Run the following two commands to generate the plots as pdf files
#    (they are stored in the same folder) 
#
#		saveBoxPlots("ETAG",c("G","I","K","M"),c("TMAX","TMIN","RAIN"));
#   savePDFs("ETAG",c("G","I","K","M"),c("TMAX","TMIN","RAIN"));
#
##################################################################


require(xts)  
require(R.oo)  
  
#
# Reads the daily timeseries of variable from myfile.
# Requires variable to be one of "TMAX", "TMIN", "RAIN"
# myfile should be a path to the .AgMIP file
#
getDaily <- function(myfile,variable="TMAX"){
	d<-read.csv(myfile, header=T, sep="", skip=4);	
	var <- xts(d[variable],as.Date(ISOdate(d$YYYY,d$MM,d$DD)));
	return(var);
	}

#	
# Converts a daily timeseries to a monthly one
# Requires ts to be an xts daily timeseries,
# method is a function (mean, max, min) or a variable (TMAX, TMIN, RAIN)  
#
toMonthly <- function(ts,method="mean"){ 
	if(method=="TMAX") method = "mean"; 
	if(method=="TMIN") method = "mean"; 
	if(method=="RAIN") method = "sum"; 
	return(apply.monthly(ts, match.fun(method)));
	}
   


#
# Prepares monthly data for certain station, rcp and variable
#
boxPlotData <- function(station, rcp="C", variable="TMAX"){
	# read the observed
	file = paste(station,"0XXX.AgMIP",sep="");
	obs = getDaily(file,variable);
	res = toMonthly(obs,variable);
	colnames(res) <- substring(file,1,8);
	
	myfiles = list.files(pattern=paste(station,rcp,"a*",sep="")); 

	for(myfile in myfiles){  
	  ts = getDaily(myfile,variable);
	  tsm = toMonthly(ts,variable);
		colnames(tsm) <- substring(myfile,1,8);
		res = merge(res,tsm);
		}
	   	
	return(list(results=res,names=as.character(c(fifth[substring(file,5,5)],
																							 sixth[substring(myfiles,6,6)]))));
}     


#
# Iterates over stations, rcps and variable and stores
#	boxplots in a single pdf file.
#
saveBoxPlots <- function(station, rcps=c("C"), vars=c("TMAX")){
	pdf(file=paste(station,"boxplots.pdf",sep="_"),paper= "a4r");
	for(variable in vars){
		for(rcp in rcps){
			d = boxPlotData(station,rcp,variable);
			ms = apply(d$results,2,median);
			res = d$results[,order(ms)];
			nam = d$names[order(ms)];		
			boxplot(as.matrix(res), names=nam, las=2, 
							cex.axis=0.8, horizontal=FALSE, cex.lab=0.75,
							 main=paste(variable,"at",station,"for",as.character(fifth[rcp])),
							 ylab=as.character(ylabs[variable]));   
		}
	}
  dev.off();
}



#
# Returns a list of GCMs available for a certain station and RCP
#
listGCMs<-function(station,rcp="C"){
		return(substring(list.files( pattern=paste(station,rcp,"a*",sep="")),6,6));
}


#
# Iterates over stations, rcps, variables and stores
#	probability density functions in a single pdf file
#
savePDFs <- function(station, rcps=c("C"), vars=c("TMAX")){
pdf(file=paste(station,"pdfs.pdf",sep="_"),paper= "a4r");
for(variable in vars){
	flag = "";
	ylimit=0.25;
	xlimit = c(5,40);
	if(variable=="RAIN") {flag="from=2.5,"; ylimit=0.125; xlimit= c(0,80)}  
	
	for(rcp in rcps){
			gcms = listGCMs(station,rcp); 
			baseline = getDaily(paste(station,"0XXX.AgMIP",sep=""),variable);
			dens = eval(parse(text=paste("density(baseline,",flag,")")));
		  plot(dens,
							ylim=c(0,ylimit),
							xlim=xlimit,
							col="black", lwd=3, 
							xlab=as.character(ylabs[variable]),
							ylab="Probability", 
							main=paste(variable,"at",station,"for",as.character(fifth[rcp])));
			
			for(gcm in gcms){
					projection = getDaily(paste(station,rcp,gcm,"XA.AgMIP",sep=""),variable); 
			    den = eval(parse(text=paste("density(projection,",flag,")")));
					lines(den,col=charToInt(gcm))
			}  
			 legend((if(variable=="RAIN") 0.8*xlimit[2] else xlimit[1]),ylimit,   #was: min(dens$x),.25,
			 c(as.character(fifth["0"]), as.character(sixth[gcms])), 
			 lty=c(1,1), 
			 lwd=c(1,1), 
			 cex=0.4,
			 col=c("black",as.character(color[gcms]))) 
	}
} 
	dev.off();
}


#
# Summary script
# Reads all .AgMIP files and stores them in a single cvs 
# Use as: 
# saveAll() 
# saveAll("ETAG") for a single station 
# saveAll("ETAGC") for a single station and scenario
# saveAll("ETAG","ETAG_all.csv")   to change the filename
#
#
saveAll<-function(starts="",filename="results.csv"){
	#reads the file names
	myfiles <- list.files(pattern=paste(starts,"[[:alnum:]]*.AgMIP",sep=""));

	#write an empty initial matrix of 13 columns
	result<-matrix(,0,13);
	
	# will loop in to individual files
	for (myfile in myfiles) {
    	temp<-read.csv(myfile, header = T, sep = "", skip = 4)
    	len = dim(temp)[1];
    	name = array(substring(myfile,1,8), c(len,1), dimnames="file");
    	temp <- cbind(name,temp);
    	result<- rbind(result,temp)
		}

		#writes the output table in to .csv
		write.table(result, file=filename, sep=",", row.names=FALSE, col.names=TRUE);
}




#
# RCP codes
# fifth character in the filename
#
fifth<-list(
"0" = "1980-2009 baseline", 
"1" = "A2-2005-2035 (Near-term)",
"2" = "B1-2005-2035 (Near-term)",
"3" = "A2-2040-2069 (Mid-Century)",
"4" = "B1-2040-2069 (Mid-Century)",
"5" = "A2-2070-2099 (End-of-Century)",
"6" = "B1-2070-2099 (End-of-Century)",
"S" = "sensitivity scenario",
"A" = "observational time period (determined in file)",
"B" = "RCP2.6 2010-2039 (Near-term)",
"C" = "RCP4.5 2010-2039 (Near-term)",
"D" = "RCP6.0 2010-2039 (Near-term)",
"E" = "RCP8.5 2010-2039 (Near-term)",
"F" = "RCP2.6 2040-2069 (Mid-Century)",
"G" = "RCP4.5 2040-2069 (Mid-Century)",
"H" = "RCP6.0 2040-2069 (Mid-Century)",
"I" = "RCP8.5 2040-2069 (Mid-Century)",
"J" = "RCP2.6 (End-of-Century)",
"K" = "RCP4.5 (End-of-Century)",
"L" = "RCP6.0 (End-of-Century)",
"M" = "RCP8.5 (End-of-Century)" );


#
# GCM codes
# sixth character in the filename
#
sixth<-list(
"0" = "imposed values (sensitivity tests)",
"A" = "ACCESS1-0", 
"B" = "bcc-csm1-1", 
"C" = "BNU-ESM", 
"D" = "CanESM2",
"E" = "CCSM4",
"F" = "CESM1-BGC",
"G" = "CSIRO-Mk3-6-0", 
"H" = "GFDL-ESM2G",
"I" = "GFDL-ESM2M",
"J" = "HadGEM2-CC",
"K" = "HadGEM2-ES",
"L" = "inmcm4",
"M" = "IPSL-CM5A-LR", 
"N" = "IPSL-CM5A-MR", 
"O" = "MIROC5",
"P" = "MIROC-ESM",
"Q" = "MPI-ESM-LR",
"R" = "MPI-ESM-MR",
"S" = "MRI-CGCM3",
"T" = "NorESM1-M"
);   

ylabs<-list(
"TMIN" = "Temperature (C)",
"TMAX" = "Temperature (C)",
"RAIN" = "Precipitation (mm) (greater than 2.5)"); 


#
# Color codes
# (from http://geography.uoregon.edu/datagraphics/color/StepSeq_25.txt)
#
color<-list( 
"A"=rgb(0.6,0.06,0.06),
"B"=rgb(0.7,0.175,0.175),
"C"=rgb(0.8,0.32,0.32),
"D"=rgb(0.9,0.495,0.495),
"E"=rgb(1,0.7,0.7),
"F"=rgb(0.6,0.33,0.06),
"G"=rgb(0.7,0.438,0.175),
"H"=rgb(0.8,0.56,0.32),
"I"=rgb(0.9,0.697,0.495),
"J"=rgb(1,0.85,0.7),
"K"=rgb(0.42,0.6,0.06),
"L"=rgb(0.525,0.7,0.175),
"M"=rgb(0.64,0.8,0.32),
"N"=rgb(0.765,0.9,0.495),
"O"=rgb(0.9,1,0.7),
"P"=rgb(0.06,0.42,0.6),
"Q"=rgb(0.175,0.525,0.7),
"R"=rgb(0.32,0.64,0.8),
"S"=rgb(0.495,0.765,0.9),
"T"=rgb(0.7,0.9,1),
"U"=rgb(0.15,0.06,0.6),
"V"=rgb(0.262,0.175,0.7),
"W"=rgb(0.4,0.32,0.8),
"X"=rgb(0.562,0.495,0.9),
"Y"=rgb(0.75,0.7,1),
"0"=rgb(0,0,0)
)