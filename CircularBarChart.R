library(dplyr)
library(ggplot2)
library(extrafont)
library(readxl)

font_import()
loadfonts(device="win")

Result <- read_excel(choose.files()) %>% 
  mutate(Percent = Count/sum(Count) * 100,
         CatPercent = paste("%", Percent, " - ", Cat, "              ", sep = "")) %>% 
  arrange(Count)

ggplot(Result, aes(x = reorder(Cat, Percent), y = Percent + 15, fill = Cat)) + 
  geom_bar(width = 0.9, stat="identity") + 
  geom_point(mapping = aes(color = Cat), size = 21) +
  geom_point(mapping = aes(y = 0, color = Cat), size = 21) +
  geom_text(data = Result, hjust = 1, vjust = -0.25, fontface = "bold", size = 5, aes(x = Cat, y = 0, label = CatPercent)) +
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  ylim(c(0,100)) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 20, family = "Segoe UI"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
