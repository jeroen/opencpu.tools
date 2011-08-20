# TODO: Add comment
# 
# Author: jeroen
###############################################################################


makefunction <- function(myfun){

	if(!is.function(myfun)){
		stop("code is not a function.")
	}
	
	#we don't care about the environment
	environment(myfun) <- globalenv();
	return(myfun);
}
