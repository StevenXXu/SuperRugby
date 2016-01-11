library(monmlp)

set.seed(123)
# load data
data <- readRDS("NRLData.rda")

as.numeric(data$Outcome)

w.mon <- monmlp.fit(as.matrix(data[,-1]),as.matrix(data[,1]),hidden1=3,monotone = 1,n.ensemble = 15,bag=TRUE)


