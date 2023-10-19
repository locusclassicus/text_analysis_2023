library(ggplot2)
library(ggimage)

data("InsectSprays")

img = "https://cdn-icons-png.flaticon.com/512/1905/1905215.png"

ggplot(data = InsectSprays, 
            aes(x = spray, y = count, 
                color = spray)) +
  geom_violin(show.legend = F) + 
  theme_dark() +
  geom_image(position = "jitter", aes(image = img), 
                      size = 0.08) +
  xlab(NULL) +
  ylab(NULL) +
  guides(colour="none")
             