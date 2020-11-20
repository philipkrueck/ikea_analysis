library(tidyverse)
library(modelr) 
library(broom) 
library(GGally) 
library(gridExtra) 
library(OutliersO3)

######################
## Protocol for DE ###
######################

tidy_ikea # make sure that tidy.R is executed and tidy_ikea is loaded into memory

# 1. Outliers X & Y

## 'Ordered scatterplots' for all variables:
tidy_ikea %>%
  mutate(across(where(is.character), as.factor)) %>% mutate(across(where(is.factor), as.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "num") %>% group_by(variable) %>%
  arrange(num) %>%
  mutate(observation = 1:n()) %>%
  ungroup() %>%
  ggplot(aes(x = num, y = observation)) + theme_bw() +
  geom_point(shape = 1, alpha = 0.5) + facet_wrap(~ variable, scales = "free_x") + ylab("") + xlab("")


## Boxplots for all factor variables against the DV (hwy):
p1 <- ggplot(tidy_ikea, aes(x = fct_lump(name, 10), y = price_eur)) + geom_boxplot(outlier.alpha = 0.3) + coord_flip() + ylab("")
p2 <- ggplot(tidy_ikea, aes(x = category, y = price_eur)) + geom_boxplot(outlier.alpha = 0.3) + coord_flip() + ylab("")
p3 <- ggplot(tidy_ikea, aes(x = sellable_online,y = price_eur))+ geom_boxplot(outlier.alpha = 0.3) + coord_flip() + ylab("")
p4 <- ggplot(tidy_ikea, aes(x = other_colors, y = price_eur)) + geom_boxplot(outlier.alpha = 0.3) + coord_flip() + ylab("")
p5 <- ggplot(tidy_ikea, aes(x = fct_lump(designer, 10), y = price_eur)) + geom_boxplot(outlier.alpha = 0.3) + coord_flip() + ylab("")
## Plot all ggplot objects together (gridExtra):
grid.arrange(p1, p2, p3, p4, p5, nrow = 3)


# 2. Homogeneity Y

tidy_ikea %>%
  ggplot(mapping = aes(x = reorder(fct_lump(name, 20), price_eur), y = price_eur, color = fct_lump(name, 5))) +
  geom_boxplot(width = 0.4, outlier.size = 0.5, outlier.alpha = 0.3, show.legend = FALSE) + 
  scale_color_manual(values = mycolors) +
  theme_minimal() +
  coord_flip() +
  labs(x = "name", y = "price in €", fill = "", title = "Homogeneity of Category and Name", subtitle = "category") +
  facet_grid(~ category)

tidy_ikea %>%
  filter(name == "HEMNES" | name == "LIDHULT" | name == "VIMLE" | name == "VALLENTUNA" | name == "GRÖNLID") %>%
  filter(category == "Beds" | category == "Chairs" | category == "Sofas & armchairs") %>%
  ggplot(mapping = aes(x = reorder(name, price_eur), y = price_eur, color = name)) +
  geom_boxplot(width = 0.4, outlier.size = 0.5, outlier.alpha = 0.3, show.legend = FALSE) + 
  scale_color_manual(values = mycolors) +
  theme_minimal() +
  coord_flip() +
  labs(x = "name", y = "price in €", fill = "", title = "Homogeneity of Category and Name", subtitle = "category") +
  facet_grid(~ category)

# 3. 

             