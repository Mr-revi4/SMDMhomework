install.packages("ggplot2")
library(ggplot2)
ggplot(data=iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(color=Species)) +
  xlab("Petal Length") +  ylab("Petal Width")
