
most.cor.in <- function(day.idx, output.idx, threshold=0.8) {
    data <- read.csv(paste("data/", day.idx, ".csv", sep=""), header=T)
    if (dim(data)[1] < 55) {
        return(NULL)
    }
    x <- data[12:55, 1:198]
    y <- data[1:44, 199:442]
    m <- cor(x, y)
    if (length(m[output.idx, m[output.idx, ]>threshold]) > 0) {
        most.cor <- sort(m[output.idx, ], decreasing=T)[1]
        if (is.na(most.cor)) {
            return(NULL)
        }
        name.cor <- names(most.cor)
        adj <- ifelse (data[55, name.cor] > data[44, name.cor], 1, -1) 
        return(adj)
    }
    return(NULL)
}

result <- read.csv("lastrec_submit.csv", header=T)
output.idx <- "O5"

for (op in 1:198) {
    output.idx <- paste("O", op, sep="")
    for (i in 201:510) {
        p <- most.cor.in(i, output.idx)
        if (is.null(p) == F) {
            #print(c(i, p))
            result[i-200, output.idx] <- result[i-200, output.idx] + 0.01 * p
        }
    }
}

write.csv(result, file=paste("curday_2hour_all_0.01_submit.csv", sep=""), 
          quote=FALSE, row.names=F)