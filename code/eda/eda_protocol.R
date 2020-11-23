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

# 3. Normality Y

tidy_ikea %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "num") %>% group_by(variable) %>%
  arrange(num) %>%
  mutate(observation = 1:n()) %>%
  ungroup() %>%
  ggplot(aes(x = num)) + theme_bw() +
  geom_histogram(bins = 100) + facet_wrap(~ variable, scales = "free") + ylab("") + xlab("")

ggplot(tidy_ikea, aes(x = price_eur)) + geom_density() +
  facet_grid(~ fct_lump_n(name, 10))

ggplot(tidy_ikea, aes(x = price_eur)) + geom_density() +
  facet_grid(~ fct_lump_n(category, 10))

ggplot(tidy_ikea, aes(x = price_eur)) + geom_density() +
  facet_grid(~ fct_lump_n(designer, 10))

# 4. Missing Values Trouble


## code taken from: https://jenslaufer.com/data/analysis/visualize_missing_values_with_ggplot.html

missing_values <- tidy_ikea %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)
## `summarise()` regrouping output by 'key', 'total' (override with `.groups` argument)
levels <-
  (missing_values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

missing_values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

# 5. Collinearity X

## corvif Function taken from Zuur et al.
## execute HighstatLib.r to load function

tidy_ikea %>% select(where(is.numeric)) %>% corvif()
tidy_ikea %>% select(where(is.numeric) & -old_price_eur) %>% corvif()

# 6. Relationships Y & X

tidy_ikea %>%
  mutate(across(where(is.character), as.factor)) %>% mutate(across(where(is.factor), as.numeric)) %>%
  # select() %>% # De-select variables here if necessary
  pivot_longer(-price_eur, names_to = "variable", values_to = "num") %>% group_by(variable) %>%
  arrange(num) %>%
  mutate(observation = 1:n()) %>%
  ungroup() %>%
  ggplot(aes(x = num, y = price_eur)) + theme_bw() +
  geom_point(shape = 1, alpha = 0.8, position = "jitter") +
  geom_smooth(se = FALSE, colour = "blue", method = "loess", formula = 'y ~ x') +
  facet_wrap(~ variable, scales = "free_x") + ylab("Price in") + xlab("")

### --> see eda_covariance.R for detailed plots

tidy_ikea %>% select(where(is.numeric)) %>%
  as.data.frame() %>%
  ggpairs(diag = list(continuous = "barDiag"))


# 7. Interactions

coplot(size_m3 ~ price_eur | fct_lump_n(designer, 5) + fct_lump_n(name, 5), data = tidy_ikea, ylab = "Size in m^3",
       xlab = "Price in €", panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y )})

coplot(size_m3 ~ price_eur | fct_lump_n(category, 5) + fct_lump_n(name, 5), data = drop_na(tidy_ikea), ylab = "Size in m^3",
       xlab = "Price in €", panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y )})

coplot(size_m3 ~ price_eur | fct_lump_n(designer, 5) + fct_lump_n(category, 5), data = drop_na(tidy_ikea), ylab = "Size in m^3",
       xlab = "Price in €", panel = function(x, y, ...) {
         tmp <- lm(y ~ x, na.action = na.omit)
         abline(tmp)
         points(x, y )})







