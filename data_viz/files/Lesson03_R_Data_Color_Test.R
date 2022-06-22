install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("ggplot2")
library(ggplot2)
display.brewer.all()
display.brewer.pal(n=8, name="Pastel1")
display.brewer.all(n = NULL, type = "div", select = NULL,
                   colorblindFriendly = TRUE)

data(iris)
head(iris, 7)

bp <- ggplot(iris, aes(Species, Sepal.Length)) +
      geom_boxplot(aes(fill=Species)) +
      theme_minimal() +
      theme(legend.position = "top")
(bp + scale_fill_brewer(palette = "Set1"))
