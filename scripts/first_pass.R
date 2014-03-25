# clearing space
rm(list=ls())

# define a couple example distance functions
Euclidean <- function(x, y) {
  return(sqrt(sum((x-y)^2)))
}

Manhattan <- function(x, y) {
  return(abs(sum(x-y)))
}

# my kNN implementation
kNN <- function(k, d.train, d.test, l.train, dist.function) {
  output <- apply(d.test, 1, FUN=function(x) {
    scores <- apply(d.train, 1, FUN=function(y) { dist.function(y, x) })
    k_rows <- order(scores)[1:k]
    label <- as.numeric(names(sort(table(l.train[k_rows]), decreasing=T))[1])
    return(label)
  })
  return(output)
}

# import data
setwd('~/Desktop/Projects/titanic-learn')
d.train.full <- read.csv(paste0(getwd(), '/raw', '/train.csv'), header=T, stringsAsFactors=F)
d.test.full <- read.csv(paste0(getwd(), '/raw', '/test.csv'))

# subset full data and the test data to something workable
d.labels <- d.train.full[, c('Survived')]
d.train.1 <- d.train.full[, c('Pclass', 'Sex')]
d.train.1$Sex <- as.numeric(d.train.1$Sex == "female")
d.train.1$Pclass <- (d.train.1$Pclass - 1) / 2

d.test.1 <- d.test.full[, c('Pclass', 'Sex')]
d.test.1$Sex <- as.numeric(d.test.1$Sex == "female")
d.test.1$Pclass <- (d.test.1$Pclass - 1) / 2

result <- as.numeric(kNN(3, d.train.1, d.test.1, d.labels, dist.function=Euclidean))
sum(is.na(result))
result <- data.frame(PassengerId=d.test.full$PassengerId, Survived=result)
write.csv(result, paste0(getwd(), '/output', '/first_pass.csv'), row.names=F)
