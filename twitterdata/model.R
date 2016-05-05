setwd("~/Downloads")
movies = read.csv("movies_v.csv")

movies$Mean
movies

plot(movies$Mean, movies$IMBD.Score)

fit <- lm(movies$Mean ~ movies$IMBD.Score)

fit2 <- lm(movies$IMBD.Score~poly(movies$Mean,2,raw=TRUE))

fit3 <- lm(movies$IMBD.Score~poly(movies$Mean,3,raw=TRUE))

fit4 <- lm(movies$IMBD.Score~poly(movies$Mean,4,raw=TRUE))

lines(sort(movies$Mean), fitted(fit)[order(movies$Mean)], col='red', type='b')


fit$fitted.values

linMap = function(x, from, to) {
  # Shifting the vector so that min(x) == 0
  x <- x - min(x)
  # Scaling to the range of [0, 1]
  x <- x / max(x)
  # Scaling to the needed amplitude
  x <- x * (to - from)
  # Shifting to the needed level
  x + from
}

score <- movies$Mean

linMap(score, 0, 10)
