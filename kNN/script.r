#library(ggplot2)
#ggplot(data=iris, aes(x = Petal.Length, y = Petal.Width)) +
  #geom_point(aes(color=Species)) +
  #xlab("Petal Length") +  ylab("Petal Width")
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

kNN <- function()
{
  orderedXl <- sortObjectsByDist(xl = iris[,3:5], z = c(1,2))
}

