trainLabels <- read.csv("trainLabels.csv")
trainLabels <- trainLabels[, -1]

output.idx <- "O5"

high.cor.in <- function(day.idx, output.idx, threshold=0.75) {
    data <- read.csv(paste("data/", day.idx, ".csv", sep=""), header=T)
    x <- data[-1, 1:198]
    x <- rbind(x, trainLabels[day.idx, ])
    y <- data[, 199:442]
    m <- cor(x, y)
    names(m[output.idx, m[output.idx, ] > threshold])
}

lt <- list()
for (i in 1:200) {
    tmp <- high.cor.in(i, output.idx)
    lt <- c(lt, tmp, recursive=T)
}
lt <- factor(lt)
most.corelated <- names(summary(lt))[1]

result <- read.csv("lastrec_submit.csv", header=T)

mod.result <- function(day.idx, output.idx) {
    data <- read.csv(paste("data/", day.idx, ".csv", sep=""), header=T)
    if (data[55, most.corelated] > data[54, most.corelated]) {
        return(result[day.idx-200, output.idx] + 0.01)
    } 
    if (data[55, most.corelated] < data[54, most.corelated]) {
        return(result[day.idx-200, output.idx] - 0.01)
    }
}

for (i in 201:510) {
    result[i-200, output.idx] <- mod.result(i, output.idx)
}
write.csv(result, file=paste("mod_", output.idx, "_0.01_submit.csv", sep=""), 
          quote=FALSE, row.names=F)
