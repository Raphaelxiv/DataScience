complete <- function(directory, id = 1:332) {
    if (list.files()[1] != "001.csv") { #here I'm checking if I'm in the right directory
        setwd(directory) #if not, I set the right directory
    }
    L<-length(id)
    Table<-data.frame("id"=id,"nobs"=0)
    for (i in 1:L) {
        I<-id[i] #I is going to be the name of the csv file I want to find the nobs
        if (1<=I && I<10) {
            I=paste0("00",as.character(I),".csv")
        }
        else if (10<=id[i] && id[i]<100) {
            I=paste0("0",as.character(I),".csv")
        }
        else {
            I=paste0(as.character(I),".csv")
        }
        # completely observed cases means that for one Date, both sulfate 
        # and nitrate have a value. That is why we are going to use complete.cases()
        Data<-read.csv(I)
        good<-complete.cases(Data[,2],Data[,3])
        Table[[i,2]]<-length(read.csv(I)[,2][good]) #This formula find the nobs
    }
    Table
}