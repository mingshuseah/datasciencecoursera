pollutantmean <- function(directory, pollutant, id=1:332) {
    files <- list.files(directory)
    meanvector <- c()
    weightvector <- c()
    for (i in id){
        current <- read.csv(files[i])
        validvector <- current[,pollutant][!is.na(current[,pollutant])]
        meanvector <- append(meanvector, mean(validvector))
        weightvector <- append(weightvector, length(validvector))
    }
    return(weighted.mean(meanvector,weightvector))
}

complete <- function(directory, id=1:332) {
    files <- list.files(directory)
    ID <- c()
    Completes <- c()
    for (i in id){
        current <- read.csv(files[i])
        total <- length(which(complete.cases(current)))
        ID <- append(ID, i)
        Completes <- append(Completes, total)
    }
    return(data.frame(ID,Completes))
}

corr <- function(directory, threshold=0){
    files <- list.files(directory)
    corrvector <- c()
    for (i in 1:length(files)){
        current <- read.csv(files[i])
        val <- current[which(complete.cases(current)),]
        if (nrow(val) > threshold){
            corrvector <- append(corrvector, cor(val$sulfate,val$nitrate))
        }
    }
    return(corrvector)
}