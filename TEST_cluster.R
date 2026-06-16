read.table(iris)
plot(iris$Sepal.Length, iris$Sepal.Width)

ggsave("/replicated/data/plot_test.pdf")
