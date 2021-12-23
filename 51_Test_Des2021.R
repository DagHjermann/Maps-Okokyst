
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 1. Check if we find referanseelver and havforsuring
#
# (run first part of 001 first, through definition of 'df_projects')  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# - referanseelver
#
sel <- grepl("referanseelv", df_projects$PROJECT_NAME, ignore.case = TRUE)
# sum(sel)
df_projects[sel,]

df_projects[sel, "PROJECT_NAME"]

df_stations_referanseelv <- get_stations_from_project(df_projects[sel, "PROJECT_NAME"], ignore.case = FALSE)  # PROJECT_ID 3320

#
# - havforsuring
#

# From Milkys project script 994:
get_project_from_onumber <- function(o_number){
  get_nivabase_selection("PROJECT_ID, O_NUMBER",
                         "PROJECTS_O_NUMBERS",
                         "O_NUMBER",
                         o_number, values_are_text = TRUE)
}

project_21 <- get_project_from_onumber("210044.OA")
# PROJECT_ID  O_NUMBER
#      12570 210044.OA

sel1 <- df_projects$PROJECT_ID == project_21$PROJECT_ID

sel2 <- grepl("havforsuring", df_projects$PROJECT_NAME, ignore.case = TRUE)
# sum(sel)
df_projects[sel2,]

sel <- sel1 | sel2

df_projects[sel,]

# Get stations from PROJECTS_STATIONS  
df_stations_oceanacid_1 <- get_nivabase_selection(
  "PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME, STATION_IS_ACTIVE",
  "PROJECTS_STATIONS",
  "PROJECT_ID",
  df_projects[sel, "PROJECT_ID"]) %>%
  left_join(
    df_projects %>% select(PROJECT_ID, PROJECT_NAME, PROJECT_DESCRIPTION, STARTDATE, ENDDATE)
  )

# Add O-number from table O_NUMBER
df_stations_oceanacid_2a <- df_stations_oceanacid_1 %>%
  left_join(
    get_nivabase_selection("PROJECT_ID, O_NUMBER",
                           "PROJECTS_O_NUMBERS",
                           "PROJECT_ID",
                           unique(df_stations_oceanacid_1$PROJECT_ID))
  )

# Add info from table STATIONS
df_stations_oceanacid_2b <- df_stations_oceanacid_2a %>%
  left_join(
    get_nivabase_selection(
      "STATION_ID, GEOM_TYPE_ID, GEOM_REF_ID, STATION_TYPE_ID",
      "STATIONS", "STATION_ID", df_stations_oceanacid_1$STATION_ID)
  )
    
# Add coordinates from table SAMPLE_POINTS
df_stations_oceanacid <- df_stations_oceanacid_2b %>%
  left_join(
    get_nivabase_selection(
      "LONGITUDE, LATITUDE, SAMPLE_POINT_ID",
      "SAMPLE_POINTS", "SAMPLE_POINT_ID", df_stations_oceanacid_2$GEOM_REF_ID),
    by = c("GEOM_REF_ID" = "SAMPLE_POINT_ID")
  ) %>%
  mutate(
    LONGITUDE = sub(",", ".", LONGITUDE) %>% as.numeric(),
    LATITUDE = sub(",", ".", LATITUDE) %>% as.numeric()
  )

df_stations_oceanacid_summ <- df_stations_oceanacid %>%
  group_by(STATION_ID) %>%
  summarise(
    across(
      c(-LONGITUDE, -LATITUDE), 
      .fns = ~paste(unique(.x), collapse = "; ")
    ),
    across(
      c(LONGITUDE, LATITUDE), 
      .fns = ~first(.x)
    )
  )

# dir.create("Data")
writexl::write_xlsx(
  list(
    `By station` = df_stations_oceanacid_summ,
    `By station x project` = df_stations_oceanacid),
  "Data/51_df_stations_oceanacid.xlsx")




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. Test maps (NOT including referanseelver and havforsuring. Independent of part 1.)
#
# Run script 01 first, to get 'df_comb2' data  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

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

