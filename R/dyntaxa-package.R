#' Access to Dyntaxa data - the Swedish Taxonomic Database - from R
#' 
#' The goal of the `dyntaxa` R package is to provide a read-only R package to interface with Dyntaxa - the taxonomic database of organisms in Sweden. 
#' 
#' The Dyntaxa data contains information about 61,500 species occurring in Sweden. This 
#' includes about 95% of known multicellular species – remaining gaps mainly 
#' found among the fungi. 
#' The scope of organisms include multicellular species documented in Sweden 
#' and such unicellular species that are included in environmental monitoring 
#' by the Swedish EPA.
#' In addition to these species there are many names at other taxonomic levels, scientific synonyms, Swedish vernaculars.
#' 
#' Citation: Liljeblad J (2019). Dyntaxa. Svensk taxonomisk databas. ArtDatabanken. Checklist dataset https://doi.org/10.15468/j43wfc.
#' @importFrom utils download.file unzip
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble 
#' @importFrom utils unzip
#' @importFrom xml2 read_xml as_list
#' @importFrom readr read_tsv
#' @importFrom purrr map_lgl
#' @name dyntaxa-package
#' @aliases dyntaxa
#' @docType package
#' @author Markus Skyttner \email{markus.skyttner@@nrm.se}
#' @keywords package


#' @importFrom utils globalVariables
# Needed for use of . in magrittr pipelines
if (getRversion() >= "2.15.1")
  globalVariables(names = unlist(strsplit(split = " ",
paste(". %>% GET arrange as_tibble bind_rows content download.file isPreferredName mutate nomenclaturalStatus recode taxonId unzip acceptedNameUsageID ancestorId creator d desc from language left_join name parentNameUsageID rankDistance scientificName scientificNameAuthorship taxonRank taxonomicStatus title to vars vernacularName link param threatStatus is_exact_match"))))

NULL