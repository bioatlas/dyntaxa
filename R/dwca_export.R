#' @noRd
#' @import xml2
dyntaxa_dwca_meta <- function() {
  
  new_meta <- as_xml_document(dyntaxa_dwca()$meta)
  
  # grab attribs from first extension
  first_ext <- new_meta %>% xml_find_first("extension")
  ext_attrs <- xml_attrs(first_ext)
  ext_attrs["rowType"] <- "http://rs.gbif.org/terms/1.0/Distribution"
  
  ext_distribution <- 
    new_meta %>% 
    xml_find_all(".//extension") %>%
    xml_parent() %>%
    xml_add_child("extension")
  
  ext_distribution %>%
    xml_add_child("files") %>%
    xml_add_child("location", "Distribution.csv") %>%
    invisible()
  
  xml_attrs(ext_distribution) <- ext_attrs
  
  ext_distribution %>% 
    xml_add_child("coreid") %>% 
    xml_set_attrs(c(index = "0"))
  
  ext_distribution %>%
    xml_add_child("field") %>%
    xml_set_attrs(c(
      index = "1", 
      term = "http://rs.tdwg.org/dwc/terms/locationID"))
              
  ext_distribution %>% 
    xml_add_child("field") %>%
    xml_set_attrs(c(
      index = "2", 
      term = "http://rs.tdwg.org/dwc/terms/countryCode"))
  
  ext_distribution %>% 
    xml_add_child("field") %>%
    xml_set_attrs(c(
      index = "3", 
      term = "http://iucn.org/terms/threatStatus")) 
  
  ext_attrs["rowType"] <- "http://rs.gbif.org/terms/1.0/Identifier"
  
  new_ext_identifier <- 
    new_meta %>% 
    xml_find_all(".//extension") %>%
    xml_parent() %>%
    xml_add_child("extension")
  
  xml_attrs(new_ext_identifier) <- ext_attrs
  
  new_ext_identifier %>%
    xml_add_child("files") %>%
    xml_add_child("location", "Identifier.csv")
  
  new_ext_identifier %>%
    xml_add_child("coreid") %>% 
    xml_set_attrs(c(index = "0"))
  
  new_ext_identifier %>% 
    xml_add_child("field") %>%
    xml_set_attrs(c(
      index = "1", 
      term="http://purl.org/dc/terms/identifier"))
              
  new_ext_identifier %>% 
    xml_add_child("field") %>%
    xml_set_attrs(c(
      index = "2", 
      term = "http://purl.org/dc/terms/title"))
  
  new_ext_identifier %>% 
    xml_add_child("field") %>%
    xml_set_attrs(c(
      index = "3", 
      term = "http://purl.org/dc/terms/format")) 
  
  new_ext_identifier %>% 
    xml_add_child("field") %>%
    xml_set_attrs(c(
      index = "4", 
      term = "http://rs.tdwg.org/dwc/terms/datasetID")) 
  
  new_meta %>% 
    xml_find_first(".")

}

#' Exports Dyntaxa data as a darwin core archive file
#' @param outfile filename to use for the darwincore archive file export
#' @return path to exported darwin core archive file
#' @importFrom rappdirs app_dir
#' @importFrom xml2 as_xml_document write_xml
#' @importFrom readr write_tsv
#' @importFrom utils zip
#' @export
#' @examples
#' \dontrun{
#'  dyntaxa_export()
#' }
dyntaxa_export <- function(outfile) {
  
  dwca_dir <- rappdirs::app_dir("dyntaxa")$config()
  dwca_file <- sprintf("dyntaxa-dwca-%s.zip", format(Sys.time(), "%Y%m%dT%H%M%S"))
  toc <- c("meta.xml", "eml.xml", "Taxon.csv", "VernacularName.csv", 
           "Reference.csv", "Distribution.csv", "Identifier.csv")
  td <- tempdir()
  t <- file.path(td, toc)
  
  dwca <- dyntaxa_dwca()
  dwca$meta <- dyntaxa_dwca_meta()

  # TODO: fix these!
  dt <- dwca$taxon_core %>% 
    mutate(acceptedNameUsageID = ifelse(acceptedNameUsageID == "1e+05", "100000", acceptedNameUsageID))
  dv <- dwca$vernacular_ext %>% 
    mutate(taxonId = ifelse(taxonId == "1e+05", "100000", taxonId))
  dr <- dwca$reference_ext
  dd <- dwca$distribution_ext %>% select(1:4) %>% 
    mutate(threatStatus = ifelse(threatStatus == "RE", "EX", threatStatus))
  di <- dwca$identifier_ext %>% filter(!taxonId %in% c("229376", "3549", "3594"))
  
  # generate dwca zip
  dwca$meta %>% write_xml(t[1], options = "format")
  dwca$eml %>% as_xml_document %>% write_xml(t[2], options = "format")
  write_tsv(dt, t[3], na = "")
  write_tsv(dv, t[4], na = "")
  write_tsv(dr, t[5], na = "")
  write_tsv(dd, t[6], na = "")
  write_tsv(di, t[7], na = "")

  destfile <- file.path(dwca_dir, dwca_file)
  if (!missing(outfile)) destfile <- outfile
  
  pwd <- getwd()
  setwd(td)
  utils::zip(destfile, toc)
  unlink(toc)
  setwd(pwd)
  
  message("Dyntaxa data exported at ", destfile)
  return (destfile)
}

#dwca_path <- dyntaxa_export()
#file.copy(dwca_path, "dyntaxa-dwca-current.zip")
