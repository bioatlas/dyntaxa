## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----eval=FALSE----------------------------------------------------------
#  library(dyntaxa)
#  
#  # the first time the package is loaded, it attempts to
#  # download and generate Dyntaxa data assets locally for use with the package
#  # and only after this, the various functions can then be used to query the Dyntaxa database
#  # this happens in the dyntaxa_init() function, which can be forced to download
#  # and overwrite the locally persisted data in order to refresh it, like so:
#  dyntaxa_init(force = TRUE)
#  

## ----basic-example-------------------------------------------------------
library(dyntaxa)

# what is the identifier for all of Animalia?
dyntaxa_id_from_name("Animalia")

# what is the rank for a given identifier?
dyntaxa_rank_from_id("5000001")

# what is the scientific name given a taxon identifier
dyntaxa_name_from_id("206046")

# show the upstream classification given the identifier for Alces alces
dyntaxa_classification(dyntaxa_id_from_name("Alces alces"))

# what are immediate children of Cervidae?
dyntaxa_children(dyntaxa_id_from_name("Cervidae"))

# what are all downstream taxa, not just the children?
dyntaxa_downstream(dyntaxa_id_from_name("Cervidae"))


## ----search-example------------------------------------------------------
library(dplyr)

# search all taxa, literature references and vernacular names at once
dyntaxa_search_all("blåklocka")


# search for hits where the taxon identifier is 220018
# note more than one hit, because we search 
# all of taxa, literature reference and vernacular name
# and there is more than one vernacular name for this taxon
dyntaxa_search_all(220018)


# so if you wish to return just the distinct set of taxon identifiers and names
dyntaxa_search_all(220018) %>% 
  distinct(taxonId, scientificName)


# search for hits with vernaculars that contain the string "räv"
# and display identifiers and names only
dyntaxa_search_vernacular("*räv*") %>%
  distinct(taxonId, scientificName, vernacularName)


# operators AND, OR, NOT can be used in searches
# search for hits where the identifier is 220018
dyntaxa_search_all("220018 AND taxonomicStatus:accepted") %>% 
  select(taxonId, scientificName, vernacularName)


# search for hits for identifier 220018
dyntaxa_search_id("220018")


# search for hits where title contains "Thomas Karlsson" and discard 
# hits that mention "Databas" in the title
dyntaxa_search_all("title:Thomas Karlsson NOT title:Databas")


# search for taxa with vernacular names matching 
# strings beginning with "fjällräv" OR "rödräv"
dyntaxa_search_vernacular("^rödräv* OR ^fjällräv*") %>% 
  distinct(taxonId, scientificName, vernacularName)



## ----typo-example--------------------------------------------------------

# this list have scientific names that are mostly misspelled
misnames <- c("Vulpe vulp", "Canis lupu", "Vulpes vulpez", "Apu apu", "Apus apus")

dyntaxa_resolve(misnames)


## ----lists-example, eval=TRUE--------------------------------------------

library(purrr)
library(dplyr)

# given a list of several species names, lookup the identifiers
lookups <- c("Abies procera", "Pinus contorta")
df <- tibble(lookups, taxonId = map_chr(lookups, dyntaxa_id_from_name))
df

# the opposite - given a list of several identifiers, lookup the names
keys <- df$taxonId
tibble(keys, scientificName = map_chr(keys, dyntaxa_name_from_id))


# combine classifications from several identifiers
res <- map(keys, dyntaxa_classification)
names(res) <- keys
bind_rows(res, .id = "key")

# combine downstream taxa for several identifiers
map(keys, dyntaxa_downstream) %>% 
  bind_rows(.id = "key")

# taxonomic immediate children for several identifiers
map(keys, dyntaxa_children) %>% bind_rows(.id = "key")

# search fulltext index for several terms
dyntaxa_search_vernacular("blåklocka OR vitsippa") %>% 
  select(taxonId, scientificName, vernacularName)

# alternatively, using purrr
terms <- paste0("vernacularName:", c("blåklocka", "vitsippa"))
map_df(terms, dyntaxa_search_all)

# find synonyms for several identifiers
keys <- c(260606, 5509)
names(keys) <- map_chr(keys, dyntaxa_name_from_id)
map(keys, dyntaxa_synonyms) %>% bind_rows(.id = "acceptedName")


## ----export-data, eval=FALSE---------------------------------------------
#  
#  # exporting data, saving locally
#  dyntaxa_export()
#  

## ----internal-data-access-example----------------------------------------

# accessing an R object directly with all the darwin core archive contents
d <- dyntaxa_dwca()

dt <- d$taxon_core
dr <- d$reference_ext
dv <- d$vernacular_ext
di <- d$identifier_ext
dd <- d$distribution_ext

#meta <- d$meta
#eml <- d$eml


## ------------------------------------------------------------------------
dt %>% head(1) %>% t()

## ------------------------------------------------------------------------
dv %>% head(1) %>% t()

## ------------------------------------------------------------------------
dr %>% head(1) %>% t()


## ----eval = FALSE--------------------------------------------------------
#  dt %>% count(taxonId) %>% arrange(desc(n))
#  dt %>% count(parentNameUsageID) %>% arrange(desc(n))
#  dt %>% count(taxonRank) %>% arrange(desc(n))
#  dt %>% count(scientificNameAuthorship) %>% arrange(desc(n))
#  dt %>% count(nomenclaturalStatus) %>% arrange(desc(n))
#  dt %>% count(taxonomicStatus) %>% arrange(desc(n))
#  
#  
#  dv %>% count(language, countryCode)
#  dv %>% filter(language == "en")
#  dv %>% count(source)
#  dv %>% count(isPreferredName)
#  dv %>% count(taxonRemarks) %>% arrange(desc(n))
#  
#  dr %>% count(title) %>% arrange(desc(n))
#  dr %>% count(date) %>% arrange(desc(date))
#  dr %>% count(taxonId) %>% arrange(desc(n))
#  
#  # taxon ids are sometimes strings and sometimes numbers
#  #dr %>% filter(taxonId == "urn:lsid:dyntaxa.se:TaxonName:153491")
#  #dr %>% filter(taxonId == 2002323)
#  

