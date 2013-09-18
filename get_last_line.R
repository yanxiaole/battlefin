readLastRecord <- function() {
    data.df <- data.frame()
    for (file in paste("data/", 1:510, ".csv", sep="")) {
        p <- read.csv(file, header=TRUE)
        data.df <- rbind(data.df, p[nrow(p), ])
    }
    rownames(data.df) <- 1:nrow(data.df)
    train.df <- data.df[1:200, ]
    write.csv(train.df, file="trainX.csv", row.names=FALSE)
    return(data.df[201:510, ])
}

readLastRecordFromFile <- function() {
    train.x <- read.csv("trainX.csv", header=TRUE)
    return(train.x)
}

readTrainTarget <- function() {
    y <- read.csv("data/trainLabels.csv", header=T)
    return(y[, -1])
}

mae <- function(y, h) {
    return(mean(abs(y - h)))
}

#test.x <- readLastRecord()
#test.x <- test.x[, 1:198]
#write.csv(test.x, file="lastrec_submit.csv", quote=FALSE)

train.x <- readLastRecordFromFile()
train.y <- readTrainTarget()
