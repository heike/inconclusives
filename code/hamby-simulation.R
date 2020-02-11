# get a new set of questioned bullets:
# one for each barrel, then five random ones 
# in practice at most 3 are from the same barrel
questioned <- function() {
  res <- c(1:10)
  five <- sample(1:10, 5, replace=TRUE)
  # if there is more than 2 bullets from the same barrel, resample
  while(any(table(five) > 2)) five <- sample(1:10, 5, replace=TRUE)
  sort(c(res,five))
}

qus <- t(replicate(20000, {
  table(factor(questioned(), levels=1:10))
}))

dframe <- data.frame(qus)
dim(unique(dframe))
# 1452 is maximal number of different sets
# pattern is 
# 5 ones, 5 twos
# 6 ones, 3 twos, 1 three
# 7 ones, 1 two, 2 threes

df <- unique(dframe)

qus <- data.frame(t(apply(df, MARGIN=1, function(x) rep(1:10, times=x))))
ds <- rowSums(qus-1)
plot(table(ds))

summary(ds)
write.csv(qus, "data/allpossible-hambysets.csv", row.names=FALSE)
