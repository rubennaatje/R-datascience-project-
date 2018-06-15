install.packages("arules")
library(arules)
library(Matrix)
library(dplyr)

compleet4 <- compleet



rmcols <- rev(seq(1,ncol(compleet4))[!as.logical(sapply(compleet4, is.numeric))])


for (i in rmcols) compleet4[[i]] <- NULL

compleet4$Gemeenten_309 <- NULL

compleet4 <- na.omit(compleet4)

mat <- data.matrix(compleet4)

test21 <- cor(compleet4,method="kendall", use="pairwise") %>%  as.data.frame 
numberaaa <- test21[2]
