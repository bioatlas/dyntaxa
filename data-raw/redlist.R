library(readr)
library(dyntaxa)
library(dplyr)

red <- 
  read_csv("~/repos/bioatlas/data-mobilization-pipeline/specieslists/red.csv") %>%
  mutate(taxonId = as.character(dyntaxa_id)) %>%
  select(-row_id) %>%
  mutate(taxonIdURI =  paste0("https://www.dyntaxa.se/Taxon/Info/", taxonId))
d <- readRDS(file.path(rappdirs::app_dir("dyntaxa")$config(), "dyntaxa.rds"))
dt <- d$taxon_core 

message("non-matches against dyntaxa, some appear to be various so called kollektivtaxon:")
message("others may be possible to recode to existing taxon identifiers?")

nonmatches <- red %>% anti_join(dt)

recode_suggestion <- function() {
  
  message(nonmatches[1, ]$taxonId, " -> ", 6003138, " : ", 
          dyntaxa_name_from_id(6003138))
  
  message(nonmatches[1, ]$taxonId, " -> ", 6003137, " : ", 
          dyntaxa_name_from_id(6003137))
  
  message(nonmatches[2, ]$taxonId, " -> ", 6008693, " : ", 
          dyntaxa_name_from_id(6008693))
  
  message(nonmatches[2, ]$taxonId, " -> ", 6008691, " : ", 
          dyntaxa_name_from_id(6008691))
  
  message(nonmatches[3, ]$taxonId, " -> ", unique(dyntaxa_search_vernacular("alfågel")$taxonId), 
          " : ", unique(dyntaxa_search_vernacular("alfågel")$vernacularName))
  
  message(nonmatches[4, ]$taxonId, " -> ", unique(dyntaxa_search_vernacular("knubbsäl")$taxonId), 
          " : ", unique(dyntaxa_search_vernacular("knubbsäl")$vernacularName))
  
  message(nonmatches[5, ]$taxonId, " -> ", 
    dyntaxa_search_name("Gautieria graveolens") %>% 
      filter(taxonomicStatus == "accepted") %>% .$taxonId %>% unique, " : ", 
      dyntaxa_search_name("Gautieria graveolens") %>% filter(taxonomicStatus == "accepted") %>%
        .$scientificName %>% unique)
  
  message(nonmatches[6, ]$taxonId, " -> ", 
          dyntaxa_id_from_name("Geastrum"), " : ", 
      dyntaxa_search_all("Geastrum") %>% filter(taxonRank == "genus") %>% .$vernacularName %>% unique)
  
  message(nonmatches[7, ]$taxonId, " -> ", 
          dyntaxa_id_from_name("Cortinarius caesiocinctus") , " : ", 
      dyntaxa_search_all("taxonId:6038616")$scientificName %>% unique)
  
  message(nonmatches[8, ]$taxonId, " -> ", 6003124, " : ", 
          dyntaxa_name_from_id(6003124))
  
  message(nonmatches[8, ]$taxonId, " -> ", 6003125, " : ", 
          dyntaxa_name_from_id(6003125))
  
  message(nonmatches[9, ]$taxonId, " -> ", 6003125, " : ", 
          dyntaxa_name_from_id(6003125))
  
  message(nonmatches[10, ]$taxonId, " -> ", 6037533, " : ", 
          dyntaxa_name_from_id(6037533))
  
  message(nonmatches[10, ]$taxonId, " -> ", 6037532, " : ", 
          dyntaxa_name_from_id(6037532))
  
  message(nonmatches[10, ]$taxonId, " -> ", 6037534, " : ", 
          dyntaxa_name_from_id(6037534))
  
  message(nonmatches[11, ]$taxonId, " -> ", 6037416, " : ", 
          dyntaxa_name_from_id(6037416))
  
  message(nonmatches[11, ]$taxonId, " -> ", 6037417, " : ", 
          dyntaxa_name_from_id(6037417))
  
  message(nonmatches[11, ]$taxonId, " -> ", 6037418, " : ", 
          dyntaxa_name_from_id(6037418))
  
  message(nonmatches[12, ]$taxonId, " -> ", 6037426, " : ", 
          dyntaxa_name_from_id(6037426))
  
  message(nonmatches[12, ]$taxonId, " -> ", 6037428, " : ", 
          dyntaxa_name_from_id(6037428))
  
  message(nonmatches[13, ]$taxonId, " -> ", 1114, " : ", 
          dyntaxa_name_from_id(1114))
  
}

recode_suggestion()

remap <- read_lines(skip_empty_rows = TRUE, "
1600 -> 6003138 : Tricholoma boreosulphurescens
1600 -> 6003137 : Tricholoma sulphurescens
66 -> 6008693 : Antrodia crassa
66 -> 6008691 : Antrodia cretacea
232124 -> 102108 : alfågel
100105 -> 102708 : knubbsäl
678 -> 202845 : Gautieria graveolens s.str.
685 -> 1000938 : jordstjärnor
3567 -> 6038616 : Cortinarius caesiocinctus
357 -> 6003124 : Clavaria atrofusca
357 -> 6003125 : Clavaria asperulispora
231301 -> 6003125 : Clavaria asperulispora
6002920 -> 6037533 : Sorbus atrata
6002920 -> 6037532 : Sorbus faohraei
6002920 -> 6037534 : Sorbus teodori
1981 -> 6037416 : Cortinarius boreidionysae
1981 -> 6037417 : Cortinarius olivaceodionysae
1981 -> 6037418 : Cortinarius dionysae
436 -> 6037426 : Cortinarius cremeiamarescens
436 -> 6037428 : Cortinarius gentianeus
208 -> 1114 : Lecanographa amylacea
3594 -> 6038764 : Rodnande spindling
3594 -> 6038766 : ?
3594 -> 6038765 : ?
3549 -> 6038752 : blek bårdspindling
3549 -> 6038751 : ?
3549 -> 6038753 : ?
229376 -> 6038845 : ?
229376 -> 6038846 : ?
")

library(stringr)

re <- "(\\w*?) -> (\\w*?) : (.*)"
newTaxonIds <- bind_cols(
  taxonId = str_replace(remap, re, "\\1"), 
  newTaxonId = str_replace(remap, re, "\\2"))




remapped_old <- 
  newTaxonIds %>% 
  left_join(nonmatches %>% 
  select(-taxonIdURI)) %>%
  mutate(taxonIdURI =  paste0("https://www.dyntaxa.se/Taxon/Info/", taxonId)) %>%
  select(-taxonId) %>%
  select(taxonId = newTaxonId, everything())

remapped <- 
  newTaxonIds %>% 
  left_join(red) %>%
  mutate(taxonIdURI =  paste0("https://www.dyntaxa.se/Taxon/Info/", taxonId)) %>%
  select(-taxonId) %>%
  select(taxonId = newTaxonId, everything())


#remapped %>% View

out <- red %>% inner_join(dt) %>% bind_rows(remapped)

out %>% count(status_abbrev)

out %>% filter(is.na(status_abbrev))

# out %>%
#   slice(1) %>%
#   select(dyntaxaID) %>%
#   .$dyntaxaID %>%
#   browseURL

distribution_ext <- 
  out %>% filter(!is.na(status_abbrev)) %>%
  select(taxonId, threatStatus = status_abbrev, 
         threatStatusEnglish = status_eng,
         threatStatusSwedish = status_swe) %>%
  mutate(countryCode = "SE", locationID = "ISO:SE") %>%
  select(taxonId, locationID, countryCode, threatStatus, everything()) %>%
  mutate(threatStatus = ifelse(threatStatus == "RE", "EX", threatStatus))

distribution_ext %>% count(threatStatus)

write_excel_csv(out, "~/repos/bioatlas/data-mobilization-pipeline/specieslists/specieslist-red.csv")

identifier_ext <- 
  dt %>% filter(taxonomicStatus == "accepted", nomenclaturalStatus == "valid") %>%
  select(taxonId) %>%
  mutate(identifier = paste0("https://www.dyntaxa.se/Taxon/Info/", taxonId)) %>%
  mutate(title = "dyntaxa.se taxonomic information") %>%
  mutate(format = "text/html") %>%
  mutate(datasetID = "https://dyntaxa.se") 

usethis::use_data(identifier_ext, distribution_ext, internal = TRUE, overwrite = TRUE)

# add gbif identifiers????

library(taxize)
library(purrr)

lookups <- 
  dt %>% 
  filter(taxonomicStatus == "accepted", nomenclaturalStatus == "valid") %>% 
#  slice(1:100) %>% 
  select(taxonId, scientificName, taxonRank)

my_gbif_id <- function(scientificName, taxonRank) {
  res <- attr(get_gbifid(sciname = scientificName, rank = taxonRank, rows = 1), "uri")  
  if (is.null(res)) return (NA)
  res
}

gbif_lookup <- purrr::possibly(my_gbif_id, NA)

gbif_id <- map2_chr(lookups$scientificName, lookups$taxonRank, gbif_lookup)

lookups$identifier <- gbif_id

lookups %>% select(taxonId, identifier) %>%
  mutate(title = "GBIF.org taxon key") %>%
  mutate(format = "text/html") %>%
  mutate(datasetID = "https://dyntaxa.se") %>%
  filter(!is.na(identifier))


