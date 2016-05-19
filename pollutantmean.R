pollutantmean <- function(directory,pollutant,id = 1:332) {
    if (list.files()[1] != "001.csv") { ##here I'm checking if I'm in the right directory
        setwd(directory) ##if not, I set the right directory
    }
    L<-as.integer(length(id))
    if (pollutant=="sulfate"){
        C<-2
    }
    else {
        C<-3
    }
    values<-numeric(0)
    for (i in 1:L) {
        if (1<=id[i] && id[i]<10) {
            X<-read.csv(paste0("00",as.character(id[i]),".csv"))
        }
        else if (10<=id[i] && id[i]<100) {
            X<-read.csv(paste0("0",as.character(id[i]),".csv"))
        }
        else {
            X<-read.csv(paste0(as.character(id[i]),".csv"))
        }
        values<-c(values,X[,C])
    }
    return(mean(values,na.rm=TRUE))
}


