
#
# Check if we find referanseelver and Havforsuring
#
# (run first part of 001 first, through definition of 'df_projects')  
#
sel <- grepl("referanseelv", df_projects$PROJECT_NAME, ignore.case = TRUE)
# sum(sel)
df_projects[sel,]

df_projects[sel, "PROJECT_NAME"]

df_stations_referanseelv <- get_stations_from_project(df_projects[sel, "PROJECT_NAME"], ignore.case = FALSE)  # PROJECT_ID 3320

sel <- grepl("havforsuring", df_projects$PROJECT_NAME, ignore.case = TRUE)
# sum(sel)
df_projects[sel,]

#
# Test maps
#
# Run script 01 first, to get 'df_comb2' data  
#

# Original
mapview(df_comb2, zcol = "Stasjon", col.regions = cols, alpha.regions = 1,
        label = label_2)

# Including 'burst'
# - too many layers!
mapview(df_comb2, zcol = "Stasjon", col.regions = cols, alpha.regions = 1,
        label = label_2, burst = TRUE)

#
# Split data set in order to get a better number of layers
#

# Test getting first part of 'Stasjon'  
# table(df_comb2$Stasjon) %>% names() %>% stringr::str_extract("[^[[:blank:]]]+")

#
# Better; Split by first part of 'Stasjon' (Bløtbunn, hardbunn etc) => 5 layers  
#
df_comb3 <- df_comb2 %>%
  mutate(Type = Stasjon %>% stringr::str_extract("[^[[:blank:]]]+"))

df_comb3_list <- df_comb3 %>%
  base::split(df_comb3$Type)  

#
# Test map with 5 layers
#
mv1 <- mapview(df_comb3_list[[1]], zcol = "Stasjon", col.regions = cols[1:2], alpha.regions = 1,
               label = label_2, layer.name = "Bløtbunn")
mv2 <- mapview(df_comb3_list[[2]], zcol = "Stasjon", col.regions = cols[3:4], alpha.regions = 1,
               label = label_2, layer.name = "Hardbunn")
mv1 + mv2

names(df_comb3_list)

#
# More systematic
#
mv <- vector("list", 5)
col_list <- list(cols[1:2], cols[3:4], cols[5:6], cols[7:8], cols[9:10])

map_list <- seq_along(df_comb3_list) %>%
  purrr::map(
    ~mapview(df_comb3_list[[.x]], zcol = "Stasjon", col.regions = col_list[[.x]], alpha.regions = 1,
          label = label_2, layer.name = names(df_comb3_list)[.x])
)

# Does NOT work:
# mapview(map_list)

map_list[[1]] + map_list[[2]] + map_list[[3]] + map_list[[4]] + map_list[[5]]

