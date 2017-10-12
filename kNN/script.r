euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}

kNN <- function(xl, z, k)
{
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}

library(ggplot2)
p <- ggplot(data=iris, aes(x = Petal.Length, y = Petal.Width)) +
geom_point(aes(color=Species)) +
xlab("Petal Length") +  ylab("Petal Width")

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
z <- c(4.5, 1.5)
xl <- iris[, 3:5]
class <- kNN(xl, z, k=7)

p + geom_point(x = z[1], y = z[2], shape = 15, size = 2, colour = colors[class])

