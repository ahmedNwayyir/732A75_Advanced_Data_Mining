library(dplyr)
library(knitr)
library(kableExtra)
df1.auto <- ISLR::Auto %>% 
  
  # recode origin as a factor 
  mutate(origin = case_when(
    origin == 1 ~ "American",
    origin == 2 ~ "European",
    origin == 3 ~ "Japanese"
  ) %>% as.factor())

df1.auto %>% 
  sample_n(15) %>% 
  kable() %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped",
                                      "condensed", 
                                      "responsive"), 
                full_width = FALSE, 
                position = "left")

set.seed(3)

df2.auto.subset <- df1.auto %>% 
  select(name,
         mpg, 
         cylinders, 
         displacement) %>%  
  sample_n(3)

# print: 
df2.auto.subset %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "condensed", 
                                      "responsive"), 
                full_width = FALSE, 
                position = "left")

# create matrix object for convenience: 
m1.auto.numeric <- select(df2.auto.subset, 
                          -name) %>% 
  as.matrix()

rownames(m1.auto.numeric) <- df2.auto.subset %>% pull(name)

m1.euclid.dis <- dist(m1.auto.numeric,
                      method = "euclidean")

m1.euclid.dis

library(cluster)
m1.gower.dis <- daisy(m1.auto.numeric, 
                      metric = "gower")

m1.gower.dis

set.seed(3)
df3.auto.subset.cat <- df1.auto %>% 
  select(name,
         mpg, 
         cylinders, 
         displacement, 
         origin) %>%  
  sample_n(3)

# print: 
df3.auto.subset.cat %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "condensed", 
                                      "responsive"), 
                full_width = FALSE, 
                position = "left")

# for convenience, add rownames: 
rownames(df3.auto.subset.cat) <- df3.auto.subset.cat %>%
  pull(name) %>% 
  substr(1,10)



m2.gower.dis <- daisy(df3.auto.subset.cat, 
                      metric = "gower")

m2.gower.dis

set.seed(3)
df4.auto.new <- df1.auto %>% 
  select(name,
         mpg, 
         cylinders, 
         displacement, 
         origin) %>%  
  sample_n(3) %>% 
  mutate(category.var.1 = as.factor(c("yes", 
                                      "yes", 
                                      "no")), 
         category.var.2 = as.factor(c("yes", 
                                      "yes", 
                                      "no")), 
         category.var.3 = as.factor(c("yes", 
                                      "yes", 
                                      "no")), 
         category.var.4 = as.factor(c("yes", 
                                      "yes", 
                                      "no")), 
         category.var.5 = as.factor(c("yes", 
                                      "yes", 
                                      "no")))

# add rownames for convenience: 
rownames(df4.auto.new) <- df4.auto.new %>%
  pull(name) %>% 
  substr(1,10)


# Calculate Gower's similarity indexes: 
m3.gower.newdata <- daisy(df4.auto.new, 
                          metric = "gower")

m3.gower.newdata


# assign rownames for convenience: 
rownames.start <- df1.auto %>%
  pull(name) %>% 
  substr(1,6)

rownames <- paste(rownames.start, 
                  1:nrow(df1.auto), 
                  sep = "-")

rownames(df1.auto) <- rownames

# find distance matrix: 
m4.auto.gower.dist <- daisy(df1.auto, 
                            metric = "gower")


# now use hclust: 
c1.auto.cluster <- hclust(m4.auto.gower.dist)

# plot dendrogram: 
# Far too many rows to visualize in one tree
# plot(c1.auto.cluster, 
#      hang = -1,
#      cex = 0.6,
#      cex.lab = 0.1)

# That's not easy to interpret. Let's try an alternative plotting method: 

d1.auto.dendrogram <- as.dendrogram(c1.auto.cluster)

# Far too many rows to visualize in one tree
plot(d1.auto.dendrogram,
     horiz = FALSE, 
     leaflab = "none")