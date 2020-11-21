
# EXPLORATORY DATA ANALYSIS
library('RColorBrewer')

tidy_ikea # make sure that tidy.R is executed and tidy_ikea is loaded into memory

num_cols <- 17
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(num_cols) # add a custom color palette

#################
## Covariance ###
#################

# price + name

top_ten_names <- fct_lump(tidy_ikea$name, 10)

tidy_ikea %>%
  ggplot(mapping = aes(x = reorder(top_ten_names, price_eur), y = price_eur, color = top_ten_names)) +
    geom_boxplot(width = 0.4, outlier.size = 0.5, outlier.alpha = 0.3, show.legend = FALSE) + 
    scale_color_manual(values = mycolors) +
    theme_minimal() +
    coord_flip() +
    labs(x = "name", y = "price in €", fill = "", title = "Price by Name")

# price + category
top_four_categories <- fct_lump(tidy_ikea$category, 4)

tidy_ikea %>%
  ggplot(aes(x = price_eur, fill = top_four_categories)) +
    geom_density(position = "fill", alpha = 0.8) +
    scale_fill_manual(values = mycolors) +
    theme_minimal() +
    labs(x = "price in €", y = "distribution in %", fill = "categories", title = "Category Density by Price") + 
    scale_y_continuous(labels = scales::percent)

tidy_ikea %>%
  ggplot(aes(x = reorder(top_four_categories, price_eur), y = price_eur, colour = top_four_categories)) +
    geom_boxplot(width = 0.4, outlier.size = 0.5, outlier.alpha = 0.3,  show.legend = FALSE) +
    scale_color_manual(values = mycolors) +
    theme_minimal() +
    labs(x = "", y = "price in €", fill = "", title = "Price Distribution per Category") +
    coord_flip()

# price + old_price

tidy_ikea %>%
  filter(!is.na(old_price_eur)) %>%
  ggplot(aes(x = old_price_eur, y = price_eur)) +
    geom_point(color = mycolors[1], alpha = 0.5) +
    geom_smooth(color = mycolors[5], se = FALSE) +
    theme_minimal() +
    labs(x = "old price in €", y = "price in €", title = "Price vs. Old Price")

# price + sellable_online

tidy_ikea %>%
  ggplot(aes(x = price_eur, y = sellable_online, color = sellable_online)) +
    geom_boxplot(width = 0.4, outlier.size = 0.5, outlier.alpha = 0.3,  show.legend = FALSE) +
    scale_color_manual(values = mycolors) +
    theme_minimal() +
    labs(x = "price in €", y = "sellable online", fill = "", title = "Price Distribution Sellable Online")

# price + other_colours

tidy_ikea %>%
  ggplot(aes(x = price_eur, y = other_colors, color = other_colors)) +
  geom_boxplot(width = 0.4, outlier.size = 0.5, outlier.alpha = 0.3,  show.legend = FALSE) +
  scale_color_manual(values = mycolors) +
  theme_minimal() +
  labs(x = "price in €", y = "other colors", fill = "", title = "Price Distribution Other Colors")

# price + designer
top_ten_designers <- fct_lump(tidy_ikea$designer, 10)

tidy_ikea %>%
  ggplot(mapping = aes(x = fct_reorder(top_ten_designers, price_eur), y = price_eur, colour = top_ten_designers)) +
    geom_boxplot(width = 0.4, outlier.size = 0.5, outlier.alpha = 0.3,  show.legend = FALSE) +
    theme_minimal() +
    labs(x = "", y = "price in €", fill = "", title = "Price Distribution per Designer") +
    coord_flip()

# price + size_m3
tidy_ikea %>%
  filter(!is.na(size_m3)) %>%
  ggplot(aes(x = size_m3, y = price_eur, colour = "red")) +
    geom_point(alpha = 0.3, show.legend = FALSE) +
    geom_smooth(show.legend = FALSE, color = "orange", fill = "orange", alpha = 0.25) +
    scale_color_manual(values = mycolors) +
    theme_minimal() +
    xlim(0, 9) + 
    labs(x = "size in m^3", y = "price in €", title = "Price by Volume")

tidy_ikea %>% # focused version of above diagram to better inspect relationship
  filter(!is.na(size_m3)) %>%
  ggplot(aes(x = size_m3, y = price_eur, colour = "red")) +
    geom_point(alpha = 0.3, show.legend = FALSE) +
    geom_smooth(show.legend = FALSE, color = "orange", fill = "orange", alpha = 0.25) +
    scale_color_manual(values = mycolors) +
    theme_minimal() +
    xlim(0, 2.5) + 
    ylim(0, 1600) +
    labs(x = "size in m^3", y = "price in €", title = "Price by Volume")

# designer + category

tidy_ikea %>%
  ggplot(aes(x = reorder(top_ten_designers, price_eur), y = category )) +
    geom_tile(aes(fill = price_eur)) +
    theme_minimal() +
    labs(x = "designer",
         y = "category",
         title = "Designer vs. Category")

# size + sellable online

tidy_ikea %>%
  filter(!is.na(size_m3)) %>%
  ggplot(aes(x = size_m3, y = sellable_online, color = sellable_online)) +
    geom_boxplot(width = 0.4, outlier.size = 0.5, outlier.alpha = 0.3,  show.legend = FALSE) +
    scale_color_manual(values = mycolors) +
    theme_minimal() +
    labs(x = "size in m^3",
         y = "item is sellable online",
         title = "Distribution of Online Sellable Items in Relation to Size")
 