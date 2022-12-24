library(dplyr)
library(ggplot2)

df <- test

# Create two new columns: group median and group residual
df1 <- df %>%
  group_by(YEAR)   %>%
  mutate( Median = median(UP),
          Residuals = sqrt( abs(time - Median)))

# Generate the s-l plot
ggplot(df1, aes(x = Median, y = Residuals)) + 
  geom_jitter(alpha = 0.4, width = 0.2) +
  stat_summary(fun.y = median, geom = "line", col = "red") +
  ylab(expression(sqrt(abs(" Residuals "))))