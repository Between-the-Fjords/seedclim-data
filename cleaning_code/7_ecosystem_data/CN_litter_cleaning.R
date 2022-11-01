### Clean CN data ###
library(janitor)
library(readxl)
library(tidyverse)

cn_raw <- read_excel("cleaning_code/7_ecosystem_data/data/Litter_CN2016.xlsx") |> 
  clean_names()

cn_data <- cn_raw |> 
  mutate(site = recode(site,
                  "Ulv"="Ulvehaugen", 
                  "Arh" = "Arhelleren",
                  "Alr"="Alrust",
                  "Fau"="Fauske",
                  "Gud"="Gudmedalen",
                  "Hog"= "Hogsete",
                  "Lav"="Lavisdalen",
                  "Ovs"= "Ovstedalen",
                  "Ram"="Rambera",
                  "Skj"= "Skjelingahaugen",
                  "Ves"="Veskre",
                  "Vik"="Vikesland"),
         year = year(date_time),
         cn_ratio = c/n) |> 
  select(year, date_time, siteID = site, sampleID = sample, sub_sampleID = name, sample_weight = weight, n_percent = n, c_percent = c, cn_ratio)

write_csv(cn_data, "cleaning_code/7_ecosystem_data/data/VCG_clean_litter_cn_2016.csv")

ggplot(cn_data, aes(x = sampleID, y = cn_ratio)) +
  geom_point() +
  facet_wrap(~siteID)
