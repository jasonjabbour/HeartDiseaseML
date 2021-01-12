Actual <- factor(c(0, 0, 1, 1))
Predicted <- factor(c(0, 1, 0, 1))
Y      <- c(16,1,4,15)
df <- data.frame(TClass, PClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), color = "white", vjust = 1) +
  scale_fill_gradient(low = "darkturquoise", high = "deepskyblue4") +
  theme_bw() + theme(legend.position = "none")