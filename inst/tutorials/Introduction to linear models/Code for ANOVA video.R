library(ggplot2)
library(cowplot)
set.seed = 21

# Generate factor with three levels
F1 <- as.factor(rep(c("A", "B", "C"), each = 250))



# Generate data from three normal distributions
# sd = 3, means  = 19, 20, 20.5 depending on the level
# of F1

var1 <- c(rnorm(250, mean = 20, sd = 3),
          rnorm(250, mean = 20, sd = 3),
          rnorm(250, mean = 20, sd = 3))


# Generate data from three normal distributions
# sd = 3, means  = 15, 20, 23 depending on the level
# of F1

var2 <- c(rnorm(250, mean = 15, sd = 3),
          rnorm(250, mean = 20, sd = 3),
          rnorm(250, mean = 23, sd = 3))


p1 <- ggplot(data = data.frame(F1, var1),
             aes(x = var1, fill = F1)) +
  xlim(5, 35) +
  geom_density(alpha = 0.6) +
  theme_bw() +
  labs(y = "Density")




p2 <- ggplot(data = data.frame(F1, var2),
             aes(x = var2, fill = F1)) +
  xlim(5, 35) +
  geom_density(alpha = 0.6) +
  theme_bw() +
  labs(y = "Density")

plot_grid(p1, p2, nrow = 2)

var(var1)
var(var2)

SStotal1 <- sum((var2 - mean(var2))^2)

SSerror1 <- sum((var2[F1 == "A"] - mean(var2[F1 == "A"]))^2) +
            sum((var2[F1 == "B"] - mean(var2[F1 == "B"]))^2) +
            sum((var2[F1 == "C"] - mean(var2[F1 == "C"]))^2)

SStreatment1 <- SStotal1 - SSerror1


A1 <- lm(var1 ~ F1)
A2 <- lm(var2 ~ F1)
