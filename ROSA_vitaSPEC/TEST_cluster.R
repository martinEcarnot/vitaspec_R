read.table(iris)
plot(iris$Sepal.Length, iris$Sepal.Width)

ggsave("plot_test.pdf")