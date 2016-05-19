corr<-function(directory, threshold=0){
    if (list.files()[1] != "001.csv") { #here I'm checking if I'm in the right directory
        setwd(directory) #if not, I set the right directory
    }
    # first, we select monitor locations where the number of completely observed cases 
    # is greater than the threshold.
    FullTable<-complete("specdata")
    SubsetTable<-subset(FullTable,nobs>threshold)
    id<-SubsetTable[,1]
    L<-length(id)
    corV<-vector("numeric",length=L)
    if (L==0) {
        return(corV)
    }
    for (i in 1:L) {
        I<-id[i] #I is going to be the name of the csv file I want to find the cor value
        if (1<=I && I<10) {
            I=paste0("00",as.character(I),".csv")
        }
        else if (10<=I && I<100) {
            I=paste0("0",as.character(I),".csv")
        }
        else {
            I=paste0(as.character(I),".csv")
        }
        # let's find the cor value of I
        Data<-read.csv(I)
        good<-complete.cases(Data[,2],Data[,3])
        corV[i]<-cor(Data[,2][good],Data[,3][good])
    }
    corV
}
