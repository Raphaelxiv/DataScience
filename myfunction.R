myfunction <- function(x) {
	x<-x+2
	mean(x)
}
factorielle <- function(n) {
	x<-1
	for (i in 1:n) {
		x<-x*i
	}
	print(x)
}


