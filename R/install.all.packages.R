# TODO: Add comment
# 
# Author: jeroen
###############################################################################

# sudo apt-get install openjdk-6-jdk


listdeb <- function(){ 
	#the system command to find r-cran-packages
	listpackages <- "apt-cache search r-cran* | grep r-cran-[a-zA-Z0-9.]*"
	
	#run it
	allpackages <- system(listpackages, intern=T);
	
	#ubuntu package has different name that CRAN package:
	customname <- grepl("--", allpackages);
	debnames1 <- sapply(strsplit(allpackages[customname], ' '), head, 1)
	crannames1 <- sapply(strsplit(allpackages[customname], '-- '), tail, 1)
	
	#ubuntu package has same name as CRAN package:
	debnames2 <- sapply(strsplit(allpackages[!customname], ' '), head, 1);
	crannames2 <- sapply(strsplit(debnames2, '-'), "[[", 3);
	
	debnames <- c(debnames1, debnames2);
	crannames <- c(crannames1, crannames2);
	return(data.frame(debname=debnames, cranname=crannames));
}

installdeb <- function(packagename){
	installcmd <- paste("sudo apt-get -y install", packagename);
	out <- system(installcmd, intern=F);
	return(as.numeric(out));
}

installalldeb <- function(){
	
	packagelist <- listdeb();
	if(nrow(packagelist)==0) return(packagelist);
	
	packagelist$success <- NA;
	for(i in 1:nrow(packagelist)){
		packagelist[i, "success"] <- installdeb(packagelist[i,"debname"])
	}	
	return(packagelist);
}

compare.packages <- function(){
	options(repos="http://cran.cnr.berkeley.edu/");
	
	installed <- installed.packages();
	available <- available.packages();
	
	wehaveit <- rownames(available) %in% rownames(installed);
	output <- data.frame(package=rownames(available), installed=wehaveit, latest=FALSE, row.names=rownames(available));
	
	cranpackages <- rownames(installed)[rownames(installed) %in% rownames(available)];	
	output[cranpackages, "latest"] <- installed[cranpackages, "Version"] == available[cranpackages,"Version"]; 
	
	return(output);
}

install.all.packages <- function(){
	options(repos="http://cran.cnr.berkeley.edu/");
	options(warn=2); #converts warnings to errors.
	
	report <- installalldeb();
	skiplist <- report$cranname;
	
	allpackages <- compare.packages();
	packagenames <- rownames(allpackages);
	
	allpackages$success <- NA;
	allpackages$info <- NA:

	for(thisname in packagenames){
		message("PACKAGE: ", thisname);	
		if(thisname %in% skiplist){
			allpackages[thisname, "success"] <- TRUE;
			allpackages[thisname, "info"] <- "skipped (debian)";
			cat("Skipping", thisname, " because it is a Debian package.\n\n");
			next;
		}
		
		if(allpackages[thisname, "latest"] == TRUE){
			allpackages[thisname, "success"] <- TRUE;
			allpackages[thisname, "info"] <- "skipped (uptodate)";
			cat("Skipping", thisname, " because it has the most recent version.\n\n");
			next;
		}
		
		mytry <- try(install.packages(thisname));
		if(class(mytry) == "try-error"){
			allpackages[thisname, "success"] <- FALSE;
			allpackages[thisname, "info"] <- unclass(mytry);			
		} else {
			allpackages[thisname, "success"] <- TRUE;
			allpackages[thisname, "info"] <- "INSTALLED";				
		}		
	}	
	options(warn=0); 
	return(allpackages);
}
