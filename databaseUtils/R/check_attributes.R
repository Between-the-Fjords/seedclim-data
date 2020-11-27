attribute_table <- tbl(con, "attributes") %>% collect()

#table field names
check_attributes <- dbListTables(con) %>% 
  set_names() %>% 
  map(dbListFields, conn = con) %>% 
  map_df(enframe, value = "field", .id = "table") %>% 
  select(-name) %>% 
  full_join(attribute_table, by = c("field" = "attribute"))

#check duplicates
check_attributes %>% 
  group_by(table, field) %>% 
  filter(n() > 1)

#check missing
check_attributes %>% filter(is.na(description))

#check extra
check_attributes %>% filter(is.na(table))


# check site/species attributes
site_species_attributes <- bind_rows(
  site = tbl(con, "site_attributes") %>% select(-siteID) %>% collect(),
  species = tbl(con, "species_attributes") %>% select(-species) %>% collect(),
  .id = "type"
) %>% 
  distinct(type, attribute) %>% 
  left_join(attribute_table, by  = "attribute") 
  
  #check duplicates
site_species_attributes %>% 
  group_by(type, attribute) %>% 
  filter(n() > 1)

#check missing
site_species_attributes %>% filter(is.na(description))

#check extra
site_species_attributes %>% filter(is.na(attribute))
