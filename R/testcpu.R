testcpu <- function(size){
	repeat{
		#one iteration should take approx 30 to 60 seconds.
		A1 <- matrix(runif(size),1e3);
		return(unclass(system.time(svd(A1))));
	}	
}