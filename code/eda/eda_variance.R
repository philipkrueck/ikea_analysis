# EXPLORATORY DATA ANALYSIS
library('RColorBrewer')

tidy_ikea # make sure that tidy.R is executed and tidy_ikea is loaded into memory

num_cols <- 17
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(num_cols) # add a custom color palette

###############
## Variance ###
###############

# 1. name

top_ten_names <- fct_lump(tidy_ikea$name, 10)

top_ten_names %>%
  summary()

tidy_ikea %>%
  group_by(name) %>%
  mutate(name_occur = n()) %>%
  ggplot() +
    geom_bar(mapping = aes(x = fct_reorder(top_ten_names, -name_occur), fill = top_ten_names), show.legend = FALSE) +
    scale_fill_manual(values = mycolors) +
    theme_minimal() +
    labs(x = "Top ten names",
         y = "# of items",
         title = "Number of Items by Name")

# 2. category
tidy_ikea %>%
  group_by(category) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  ggplot(mapping = aes(x = fct_reorder(category, count), y = count, fill = category)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = count, y = count), nudge_y = 25, alpha = 0.5, size = 3) +
    theme_minimal() +
    scale_fill_manual(values = mycolors) +
    labs(x = "category", y = "# of items in category", title = "Number of Items per Category") +
    coord_flip()

# 3. price_eur
ggplot(data = tidy_ikea) +
  geom_histogram(mapping = aes(x = price_eur), binwidth = 30, fill = mycolors[1]) +
  labs(x = "price in €", y = "# of items with price", title = "Number of Items with Price") 

tidy_ikea %>%
  select(price_eur) %>%
  summary()


# 4. old_price
tidy_ikea %>%
  filter(!is.na(old_price_eur)) %>%
  ggplot() +
    geom_histogram(mapping = aes(x = old_price_eur), binwidth = 30, fill = mycolors[1]) +
    theme_minimal() +
    labs(x = "old price in €", y = "# of items with old price", title = "Number of Items with Old Price") 

tidy_ikea %>%
  select(old_price_eur) %>%
  summary() 

# percentage of missing values for old_price_eur
total <- tidy_ikea %>%
  count()

old_price_na_count <- tidy_ikea %>%
  filter(!is.na(old_price_eur)) %>%
  count()

(percent_na_old_price_eur <- as.double(round((old_price_na_count / total * 100), 2)))

# 5. sellable_online

tidy_ikea %>%
  summarize(sellable = n_true <- length(tidy_ikea$sellable_online[sellable_online == "TRUE"]),
            not_sellable = length(tidy_ikea$sellable_online[sellable_online == "FALSE"]),
            sellable_ratio = n_true / n())

# 6. other_colors
ggplot(data = tidy_ikea) +
  geom_bar(mapping = aes(x = other_colors, fill = other_colors), show.legend = FALSE) +
  scale_fill_manual(values = mycolors) +
  theme_minimal() +
  labs(x = "other colors", y = "# of items", title = "Number of Items with Other Colors") 

tidy_ikea %>%
  select(other_colors) %>%
  summary()

# 7. designer
top_ten_designers <- fct_lump(tidy_ikea$designer, 10)

tidy_ikea %>%
  ggplot(mapping = aes(x = top_ten_designers, fill = top_ten_designers)) +
    geom_bar(show.legend = FALSE) +
    theme_minimal() +
    labs(x = "designer", y = "# of items", title = "Number of Items by Designer") +
    coord_flip()

# 8. size_m3
tidy_ikea %>%
  filter(!is.na(size_m3)) %>%
  ggplot() +
    geom_histogram(mapping = aes(x = size_m3), binwidth = 0.3, fill = mycolors[1]) +
    theme_minimal() +
    labs(x = "size in m^3", y = "# of items", title = "Number of Items by Size")

tidy_ikea %>%
  filter(!is.na(size_m3)) %>%
  ggplot() +
    geom_histogram(mapping = aes(x = size_m3), binwidth = 0.3, fill = mycolors[1]) +
    xlim(0, 10)
    theme_minimal() +
    labs(x = "size in m^3", y = "# of items", title = "Number of Items by Size")

tidy_ikea %>%
  select(size_m3) %>%
  summary

tidy_ikea %>%
  group_by(category) %>%
  summary(count = count())

