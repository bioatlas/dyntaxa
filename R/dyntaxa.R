dwca_download <- function(url) {
  dwca_file <- tempfile(pattern = "dwca-", fileext = ".zip")
  dwca_dir <- dirname(dwca_file)
  if (download.file(url, destfile = dwca_file, mode = "wb", quiet = TRUE) != 0)
    stop("External error downloading from ", url)
  return (dwca_file)
}

dwca_parse_dyntaxa <- function(dwca_file) {

  # TODO use library(finch) in the next iteration

  stopifnot(file.exists(dwca_file), tools::file_ext(dwca_file) == "zip")
  filez <- unzip(dwca_file, list = TRUE, junkpaths = TRUE)$Name
  meta <- eml <- taxon <- vernacular <- reference <- NULL

  if ("meta.xml" %in% filez) {
    # TODO parse meta.xml for encoding, field/line terminators,
    # quoting char, skiplines etc and use this in further parsing
    meta <- xml2::read_xml(unz(dwca_file, "meta.xml")) %>%
      xml2::as_list()
  } else {
    warning("No meta.xml found in ", dwca_file)
  }

  if ("eml.xml" %in% tolower(filez)) {
    eml <- xml2::read_xml(unz(dwca_file, "eml.xml")) %>%
      xml2::as_list()
  } else {
    warning("No eml.xml found in ", dwca_file)
  }

  if (all(c("VernacularName.csv", "Taxon.csv", "Reference.csv") %in% filez)) {
    taxon <- suppressWarnings(suppressMessages(
      readr::read_tsv(unz(dwca_file, "Taxon.csv"), col_types = "ccccccccc")))
    vernacular <- suppressWarnings(suppressMessages(
      readr::read_tsv(unz(dwca_file, "VernacularName.csv"), col_types = "ccccccc")))
    reference <- suppressWarnings(suppressMessages(
      readr::read_tsv(unz(dwca_file, "Reference.csv"), col_types = "cccc")))
  
    # https://github.com/gbif/ipt/wiki/BestPracticesChecklists says dc:source - link to source web page
    dv <- vernacular %>%
      mutate(taxonId = as.character(taxonId)) %>%
      mutate(source = recode(source, "Dyntaxa. Svensk taxonomisk databas" = "https://dyntaxa.se")) %>%
      mutate(source = paste0("https://www.dyntaxa.se/Taxon/Info/", taxonId)) %>%
      mutate(isPreferredName = ifelse(isPreferredName == "true", TRUE, FALSE)) %>%
      arrange(taxonId)
    
    dr <- reference %>% 
      mutate(date = ifelse(date == 0, NA, date)) %>%
      arrange(taxonId)

    # attempt to map to http://rs.gbif.org/vocabulary/gbif/nomenclatural_status.xml
    dt <- taxon %>% 
      mutate_at(vars(acceptedNameUsageID, parentNameUsageID), as.character) %>%
      mutate(nomenclaturalStatus = recode(nomenclaturalStatus, 
        "Correct" = "valid",
        "Misspelled" = "orthographia",
        "Incorrect (other)" = "negatum",
        "Incorrect citation" = "negatum",
        "Occupied/unneccessary" = "superfluum",
        "Suppressed" = "oppressa",
        "Undescribed" = "nudum",
        "Provisional" = "provisorium",
        "Preliminary suggestion" = "provisorium"
      )) %>%
      mutate(nomenclaturalStatus = ifelse(nomenclaturalStatus %in% 
        c("Informal", "Observation system name", "Unpublished"), NA, nomenclaturalStatus))
    
    # add root node to connect kingdoms to
    dt <- dt %>% bind_rows(tibble(
        taxonId = "0",
        acceptedNameUsageID = "0",
        parentNameUsageID = NA,
        scientificName = "Biota",
        taxonRank = "vitae",
        scientificNameAuthorship = NA,
        taxonomicStatus = "accepted",
        nomenclaturalStatus = "valid",
        taxonRemarks = NA)) %>%
      arrange(taxonId)
    
    # connect unrooted kingdom nodes -> set Biota to be parent for these nodes
    idx <- which(dt$taxonId %in%  
      c("5000001", "5000039", "5000045", "5000052", 
        "5000055", "5000060", "5000082", "5000083", "6011679", 
        "urn:lsid:dyntaxa.se:TaxonName:37551", 
        "urn:lsid:dyntaxa.se:TaxonName:37628"))

    dt <- dt %>% 
      mutate(parentNameUsageID = replace(parentNameUsageID, idx, 0))

  } else {
    stop("Some files are missing from the darwin core archive ", dwca_file)
  }

  res <- list(
    "eml" = eml,
    "meta" = meta,
    "taxon_core" = as_tibble(dt),
    "vernacular_ext" = as_tibble(dv),
    "reference_ext" = as_tibble(dr),
    "identifier_ext" = identifier_ext,
    "distribution_ext" = distribution_ext
  )

  n_probs <- function(x) {
    probs <- attr(suppressWarnings(x), "problems")
    if (is.null(probs))
        0
    else nrow(probs)
  }

  tables_with_probs <- purrr::map_lgl(res,
    function(x) n_probs(x) > 0
  )

  if (any(tables_with_probs)) {
    res$has_parsing_issues <- TRUE
    res$parsing_issues <- purrr::map(res[tables_with_probs], readr::problems) %>%
      bind_rows(.id = "table")
    res$parsing_issues$file <- dwca_file
    warning("Found parsing issues in ", dwca_file,
      ", details are in result$parsing_issues", "\n")
  } else {
    res$parsing_issues <- NULL
    res$has_parsing_issues <- FALSE
  }

  return (res)
}

# get EML at IPT and generate suitable citation
eml_download_ipt <- function(url) {
  tmp <- tempfile(pattern = "sampling-event-eml-", fileext = ".xml")
  if (download.file(url = url, destfile = tmp, quiet = TRUE, mode = "wb") != 0)
    stop("External error downloading EML from ", url)
  meta <- xml2::read_xml(tmp)

  gbif_citation <-
    meta %>%
    xml2::xml_find_first(xpath = "//citation") %>%
    xml2::xml_text()

  citation <- gsub("GBIF.org", paste(url), gbif_citation)

  res <- list(
    eml = meta,
    citation = citation
  )

  return (res)
}

eml_download_gbif <- function() {
  content(GET(
    "https://api.gbif.org/v1/dataset/de8934f4-a136-481c-a87a-b0b202b80a31/document"
  ))
}

#' Downloads a Dyntaxa database packaged as a Darwin Core Archive from https://api.artdatabanken.se/taxonservice 
#'
#' @return a character string with the path to a file containing the dwca
#' @noRd
#'
dyntaxa_download <- function() {
  dwca_download(paste0(
    "https://api.artdatabanken.se/taxonservice/v1/DarwinCore/",
    "DarwinCoreArchiveFile?Subscription-Key=4b068709e7f2427d9fc76bf42d8e2b57"
  ))
}


#' Parses a downloaded Dyntaxa database packaged as a Darwin Core Archive.
#' @param file character vector to file containing darwin core archive of the Dyntaxa database
#' @return A list with slots for metadata (`meta`), EML (`EML`) and with individual data tibbles
#' for taxon_core (Taxon), vernacular_ext (Vernacular Names) and reference_ext (Literature Reference) data
#' @noRd
#'
dyntaxa_parse <- function(file) {
  dwca_parse_dyntaxa(file)
}

#' Retrieve the taxonomic "upstream" classification given a taxon identifier
#' @param taxon_id taxon identifier/key
#' @return tibble with the columns: name, rank, and id.
#' @export
#' @importFrom rappdirs app_dir
#' @importFrom igraph subcomponent V
#' @importFrom dplyr inner_join select mutate filter arrange distinct
#' @importFrom tibble tibble
#' @examples
#' \dontrun{
#' dyntaxa_classification(dyntaxa_id_from_name("Cervidae"))
#' }
dyntaxa_classification <- function(taxon_id) {
  
  d <- dyntaxa_rds
  g <- d$graph
  dt <- d$taxon_core
  ancestors <- rev(names(subcomponent(g, V(g)[name == taxon_id], "in")))
  
  tibble(nodeId = taxon_id, ancestorId = rev(ancestors)) %>% 
    inner_join(dt, by = c("ancestorId" = "taxonId")) %>%
    select(taxonId = ancestorId, scientificName, parentNameUsageID, taxonRank) %>%
    mutate(taxonRank = ifelse(taxonRank == "unranked", NA, taxonRank)) %>%
    mutate(rankDistance = as.integer(factor(taxonRank, levels = unique(taxonRank), ordered = TRUE)) - 1) %>%
    filter(rankDistance >= 0) %>% 
    arrange(rankDistance)
}

#' Retrieve immediate descendents, ie only children at the rank one step below that of a given taxon
#' @param taxon_id character string with taxon identifier/key
#' @return tibble with the columns: childtaxa_id,  childtaxa_name,
#' @export
#' @examples
#' \dontrun{
#' dyntaxa_children(dyntaxa_id_from_name("Agaricales"))
#' }
dyntaxa_children <- function(taxon_id) {
  dyntaxa_downstream(taxon_id) %>% filter(rankDistance == 1)
}

#' Retrieve all taxa "downstream" of a given taxon identifier, descending recursively through all available taxonomic ranks
#' @param taxon_id taxon identifier/key
#' @return tibble with the columns: childtaxa_id, childtaxa_name and rank. 
#' @importFrom rappdirs app_dir
#' @importFrom igraph subcomponent V
#' @importFrom dplyr inner_join select mutate filter arrange distinct
#' @importFrom tibble tibble
#' @export
#' @examples
#' \dontrun{
#' dyntaxa_downstream(dyntaxa_id_from_name("Asteraceae"))
#' }
dyntaxa_downstream <- function(taxon_id) {
  
  d <- dyntaxa_rds
  g <- d$graph
  dt <- d$taxon_core %>% filter(taxonomicStatus == "accepted", nomenclaturalStatus == "valid")
  ancestors <- rev(names(subcomponent(g, V(g)[name == taxon_id], "out")))
  
  tibble(nodeId = taxon_id, ancestorId = rev(ancestors)) %>% 
    inner_join(dt, by = c("ancestorId" = "taxonId")) %>%
    select(taxonId = ancestorId, scientificName, parentNameUsageID, taxonRank) %>%
    mutate(taxonRank = ifelse(taxonRank == "unranked", NA, taxonRank)) %>%
    mutate(rankDistance = as.integer(factor(taxonRank, levels = unique(taxonRank), ordered = TRUE)) - 1) %>%
    #filter(rankDistance > 0) %>% 
    arrange((rankDistance))
}

#' Get synonyms given a specific taxon identifier
#' @param taxon_id taxon identifier/key
#' @return tibble with columns for identfier, name, rank and status
#' @export
#' @examples
#' \dontrun{
#' dyntaxa_synonyms(5509)
#' }
dyntaxa_synonyms <- function(taxon_id) {

  d <- dyntaxa_rds
  dt <- d$taxon_core
  
  dt %>% filter(nomenclaturalStatus == "valid") %>% 
    filter(taxonomicStatus == "synonym") %>% 
    filter(acceptedNameUsageID == taxon_id) %>% 
    distinct(scientificName, taxonomicStatus, nomenclaturalStatus)
  
}


#' Resolve scientific names that may be misspelled through fuzzy matching
#' 
#' @param taxon_name taxonomic name or vector of taxonomic names
#' @return tibble with the taxonomic name used in the query, a flag to indicate if it is an exact match (as opposed to a fuzzy match) and additional taxonomic data from the best taxon name that was matched (or NA if no match could be made)
#' @export
#' @importFrom stringdist amatch
#' @importFrom dplyr bind_cols everything left_join mutate select
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @examples 
#' \dontrun{
#' dyntaxa_resolve(c("Vulpe vulp", "Canis lupu", "Vulpes vulpez"))
#' }
dyntaxa_resolve <- function(taxon_name) {

  query <- taxon_name
  dt <- dyntaxa_rds$taxon_core

  indexes <- amatch(query, dt$scientificName, method = "jw")
  fuzzymatches <- dt$scientificName[indexes]

  ids <- map_chr(fuzzymatches, dyntaxa_id_from_name)

  tibble(query) %>% 
  bind_cols(taxonId = ids) %>% 
  left_join(dt, by = "taxonId") %>%
  mutate(is_exact_match = (query == scientificName)) %>%
  select(query, is_exact_match, taxonId, scientificName, 
         taxonRank, taxonomicStatus, nomenclaturalStatus)

}

#' Get redlist data for Dyntaxa
#' @return tibble with redlist data
#' @export
#' @examples
#' \dontrun{
#' dyntaxa_redlist()
#' }
dyntaxa_redlist <- function() {

  d <- dyntaxa_rds
  dd <- d$distribution_ext
  
  dd 
}

#' Access internal package R object representing the darwin core archive data object for the Dyntaxa database
#' @return object with slots for the darwin core archive data
#' @export
#' @examples
#' \dontrun{
#' d <- dyntaxa_dwca()
#' dt <- d$taxon_core
#' dt
#' }
dyntaxa_dwca <- function() {
  d <- dyntaxa_rds
  d
}


#' Get taxon identifiers from taxon name
#' @param taxon_name Vector of taxon names
#' @return vector of identifiers
#' @export
#' @examples
#' \dontrun{
#' dyntaxa_id_from_name("Vulpes vulpes"))
#' }
dyntaxa_id_from_name <- function(taxon_name) {
  #term <- paste0("\"", taxon_name, "\"")
  #res <- dyntaxa_search_name(term) %>% .$taxonId %>% unique
  
  res <- dyntaxa_dwca()$taxon_core %>% 
    filter(scientificName == taxon_name) %>% 
    filter(taxonomicStatus == "accepted") %>%
    .$taxonId %>% unique
  
  if (length(res) > 1) {
    message("returning only first of several matches, use ",
      sprintf("dyntaxa_search_name(\"%s\") to show all matches", taxon_name))
    return(res[1])
  }
  if (length(res) < 1) res <- NA
  res
}

#' Search full text search index of Dyntaxa data for scientific name using a search pattern
#' @seealso \url{http://www.sqlitetutorial.net/sqlite-full-text-search/} for search pattern syntax details
#' @param taxon_name search pattern(s) for scientific name
#' @return tibble with columns for identfier, name, rank and status
#' @export
#' @examples
#' \dontrun{
#' dyntaxa_search_name("\"Alces alces\" OR alces")
#' }
dyntaxa_search_name <- function(taxon_name) {
  file <- file.path(app_dir("dyntaxa")$config(), "dyntaxa.sqlite")
  src <- src_sqlite(file)
  #query <- sprintf("select * from fulltext where scientificName match '%s'", taxon_name)
  query <- sprintf("select * from fulltext 
    where scientificName match '%s' 
    order by matchinfo(fulltext, 'l') asc", taxon_name)
  src %>% tbl(sql(query)) %>% collect() %>% 
    select(taxonId, scientificName, taxonRank, taxonomicStatus, nomenclaturalStatus) %>% 
    distinct()
}

#' Search full text search index of Dyntaxa data for taxon key(s)/identifier(s) using a search pattern
#' @seealso \url{http://www.sqlitetutorial.net/sqlite-full-text-search/} for search pattern syntax details
#' @param taxon_id search term(s) specifying key/identifier
#' @return tibble with columns for identfier, name, rank and status
#' @export
#' @examples
#' \dontrun{
#' dyntaxa_search_id("5000001")
#' dyntaxa_search_id("206046 OR 206026")
#' dyntaxa_search_id("20604*")
#' }
dyntaxa_search_id <- function(taxon_id) {
  file <- file.path(app_dir("dyntaxa")$config(), "dyntaxa.sqlite")
  src <- src_sqlite(file)
  query <- sprintf("select * from fulltext where taxonId match '%s'", taxon_id)
  src %>% tbl(sql(query)) %>% collect() %>% 
    select(taxonId, scientificName, taxonRank, taxonomicStatus, nomenclaturalStatus) %>% 
    distinct()
}

#' Search full text search index of Dyntaxa data for vernacular name(s) using a search pattern
#' @seealso \url{http://www.sqlitetutorial.net/sqlite-full-text-search/} for search pattern syntax details
#' @param vernacularname search pattern(s) for vernacular name
#' @return tibble with columns for identfier, name, rank and status
#' @export
#' @examples
#' \dontrun{
#' dyntaxa_search_vernacular("lo OR fox")
#' dyntaxa_search_vernacular("varg*")
#' dyntaxa_search_vernacular("^bear*")
#' dyntaxa_search_vernacular("^vildsvinss*")
#' }
dyntaxa_search_vernacular <- function(vernacularname) {
  file <- file.path(app_dir("dyntaxa")$config(), "dyntaxa.sqlite")
  src <- src_sqlite(file)
  query <- sprintf("select * from fulltext where vernacularName match '%s'", vernacularname)
  src %>% tbl(sql(query)) %>% collect() %>% 
    select(taxonId, scientificName, vernacularName, 
           taxonRank, taxonomicStatus, nomenclaturalStatus) %>% 
    distinct()
}



#' Get scientific name given key/identifier
#' @param taxon_id search term for key/identifier
#' @return vector of names
#' @export
#' @examples
#' \dontrun{
#' dyntaxa_name_from_id(206041)
#' dyntaxa_name_from_id(206043)
#' }
dyntaxa_name_from_id <- function(taxon_id) {
  res <- dyntaxa_search_id(taxon_id) %>% .$scientificName
  if (length(res) < 1) res <- NA
  return (res)
}


#' Get ranks for a specific taxon identifier
#' @param taxon_id taxon identifier/key
#' @return rank as character or NA 
#' @export
#' @examples
#' \dontrun{
#' dyntaxa_rank_from_id("5000001")
#' }
dyntaxa_rank_from_id <- function(taxon_id) {
  res <- dyntaxa_dwca()$taxon_core %>% 
    filter(taxonId == taxon_id) %>%
    select(taxonId, taxonRank) %>%
    .$taxonRank
  if (length(res) < 1) res <- NA
  return (res)
}

#' Get links given a specific taxon identifier
#' @param taxon_id taxon identifier/key
#' @return tibble with info links 
#' @importFrom utils URLencode
#' @export
#' @examples
#' \dontrun{
#' link <- dyntaxa_links("5000001")$links[1]
#' browseURL(link)
#' }
dyntaxa_links <- function(taxon_id) {
  
  links <- readr::read_csv(trim_ws = TRUE, col_names = c("url", "title"), 
    "https://www.dyntaxa.se/Taxon/Info/%s, Taxonomic Information (Dyntaxa)
    http://artfakta.artdatabanken.se/taxon/%s, Species Information (ArtDatabanken)
    http://www.artportalen.se/media/taxon/%s, Images (Artportalen)
    https://www.nobanis.org/search-alien-species/?SpeciesQuery=%s, Alien Species (NOBANIS)
    https://dina-web.net/naturalist/search/%s?lang=sv_SE, Naturforskaren (NRM)
    http://www.eol.org/search/%s, Encyclopedia of Life
    http://www.biodiversitylibrary.org/name/%s, Biodiversity Heritage Library
    http://sv.wikipedia.org/wiki/%s, Wikipedia
    http://www.google.se/search?&amp;ie=UTF-8&amp;q=%s, Google")

  
  tn <- dyntaxa_name_from_id(taxon_id)
  tr <- dyntaxa_rank_from_id(taxon_id)

  links$param <- taxon_id
  if (!is.na(tn))
    links[3:nrow(links), ]$param <- URLencode(tn)
  
  # col <- attr(which = "uri", x = taxize::get_colid(
  #   sciname = tn, rank = tr))
  # 
  # eol <- attr(which = "uri", x = taxize::get_eolid(
  #   sciname = tn))
  # 
  # gbif <- attr(which = "uri", x = taxize::get_gbifid(
  #   sciname = tn, rank = tr))
  # 
  # wiki <- attr(which = "uri", x = taxize::get_wiki(
  #   x = tn))
  # 
  # extra <- tibble(
  #   title = c("COL", "EOL", "GBIF", "WikiSpecies"), 
  #   link = c(col, eol, gbif, wiki)
  # )
  # 
  
  links %>% 
  mutate(link = sprintf(url, param)) %>%
  mutate(taxonId = taxon_id) %>%
  select(title, link) #%>%
  # bind_rows(extra)

}


#' Search full text search index of Dyntaxa data across all data 
#' (identifiers, names, literature references, vernacular names etc)
#' using a search pattern.
#' @seealso \url{http://www.sqlitetutorial.net/sqlite-full-text-search/} 
#' for search pattern syntax details
#' @param term search terms to use
#' @return tibble with search hits
#' @export
#' @importFrom rappdirs app_dir
#' @importFrom dplyr tbl sql src_sqlite collect
#' @examples
#' \dontrun{
#' dyntaxa_search_all("vitsippa AND Aronsson")
#' dyntaxa_search_all("Fungal Diversity")
#' }
dyntaxa_search_all <- function(term) {
  file <- file.path(app_dir("dyntaxa")$config(), "dyntaxa.sqlite")
  src <- src_sqlite(file)
  query <- sprintf("select * from fulltext 
    where fulltext match '%s' order by 
    matchinfo(fulltext, 'l') asc", term)
  src %>% tbl(sql(query)) %>% collect()
}


#' @noRd
dyntaxa_init_dwca <- function(force) {
  
  dyntaxa_dwca <- file.path(app_dir("dyntaxa")$config(), "dyntaxa.zip")

  if (!dir.exists(dirname(dyntaxa_dwca)))
    dir.create(dirname(dyntaxa_dwca), recursive = TRUE)
  if (!file.exists(dyntaxa_dwca) || force) {
    message("Downloading Dyntaxa darwin core archive to ", dyntaxa_dwca)
    dl <- dyntaxa_download()
    file.copy(dl, dyntaxa_dwca, overwrite = TRUE)
  }
}


#' Generate full text search index for Dyntaxa in sqlite3 database
#' @importFrom dplyr src_sqlite copy_to left_join select
#' @importFrom DBI dbExistsTable dbRemoveTable dbSendQuery dbClearResult dbDisconnect dbConnect dbWriteTable
#' @importFrom RSQLite SQLite
#' @param force boolean to indicate forced update
#' @noRd
dyntaxa_init_sqlite <- function(force) {

  dyntaxa_db <- file.path(app_dir("dyntaxa")$config(), "dyntaxa.sqlite")
  
  if (file.exists(dyntaxa_db) && !force) {
    message("Dyntaxa full text search index already exists at: ", dyntaxa_db)
    return (invisible(TRUE))
  }
  
  message("Generating full text search index for Dyntaxa at ", dyntaxa_db)
  
  if (!dir.exists(dirname(dyntaxa_db)))
    dir.create(dirname(dyntaxa_db))
  
  dyntaxa_file <- file.path(app_dir("dyntaxa")$config(), "dyntaxa.zip")
  if (!file.exists(dyntaxa_file)) stop("Cannot find ", dyntaxa_file)
  d <- dyntaxa_parse(dyntaxa_file)
  
  my_db <- src_sqlite(dyntaxa_db, create = TRUE)
  
  copy_to(my_db, d$taxon_core, "taxon_core",
          temporary = FALSE, overwrite = TRUE,
          indexes = list(c("taxonId")))
  
  copy_to(my_db, d$reference_ext, "reference_ext",
          temporary = FALSE, overwrite = TRUE,
          indexes = list(c("taxonId")))
  
  copy_to(my_db, d$vernacular_ext, "vernacular_ext",
          temporary = FALSE, overwrite = TRUE,
          indexes = list(c("taxonId")))
  
  message("creating full text search index...")
  
  ddl <- "CREATE VIRTUAL TABLE fulltext USING fts4(taxonId, scientificName, 
    taxonRank, scientificNameAuthorship, taxonomicStatus, 
    nomenclaturalStatus, vernacularName, language, title, creator);"
    
  mydb <- dbConnect(RSQLite::SQLite(), dyntaxa_db)
  
  if (dbExistsTable(mydb, "fulltext"))
    dbRemoveTable(mydb, "fulltext")
  rs <- dbSendQuery(mydb, ddl)
  dbClearResult(rs)
  
  fulltext <- 
    d$taxon_core %>% 
    left_join(d$vernacular_ext, by = c("taxonId")) %>%
    left_join(d$reference_ext, by = c("taxonId")) %>% 
    select(taxonId, scientificName, taxonRank, scientificNameAuthorship, taxonomicStatus, 
           nomenclaturalStatus, vernacularName, language, title, creator)
  
  dbWriteTable(mydb, "fulltext", fulltext, append = TRUE)
  dbDisconnect(mydb)

}

#' Generate a list of tables with data, metadata and a graph 
#' with relations from the Dyntaxa database and persist this on disk
#' 
#' @importFrom dplyr bind_rows select mutate rename mutate_at filter recode
#' @importFrom igraph graph_from_data_frame is_dag
#' @param force boolean to incidate forced update
#' @noRd
dyntaxa_init_rds <- function(force) {

  dyntaxa_file <- file.path(app_dir("dyntaxa")$config(), "dyntaxa.zip")
  
  if (!file.exists(dyntaxa_file)) stop("Cannot find ", dyntaxa_file)
  
  d <- dyntaxa_parse(dyntaxa_file)
  
  rds_file <- file.path(app_dir("dyntaxa")$config(), "dyntaxa.rds")
  
  if (file.exists(rds_file) && !force) {
    message("Dyntaxa graph already exists at: ", rds_file)
    return (invisible(TRUE))
  }

  message("Storing dyntaxa graph relations in", rds_file)
  
  taxon <- d$taxon_core %>% 
  #  filter(taxonomicStatus == "accepted") %>%
    select(taxonId, parentNameUsageID, scientificName, taxonRank)

  # v <- bind_rows(taxon, tibble(
  #     taxonId = "0", 
  #     parentNameUsageID = NA, 
  #     scientificName = "Biota", 
  #     taxonRank = "unranked"
  #   )) 
  v <- taxon

  e <- v %>% select(from = 2, to = 1)  %>% filter(!is.na(from)) %>%
    mutate_at(vars(to, from), as.character)

  g <- graph_from_data_frame(d = e, vertices = v, directed = TRUE)

  if (!is_dag(g)) 
    warning("There are cycles! Warning!")

  d$graph <- g
    
  saveRDS(d, file = rds_file)
  
}

#' Download Dyntaxa darwin core archive and 
#' prepare data for local use with this R package
#' @param force boolean to indicate a forced update
#' @importFrom dplyr vars
#' @export
dyntaxa_init <- function(force = FALSE) {

  dyntaxa_init_dwca(force)
  dyntaxa_init_rds(force)
  dyntaxa_init_sqlite(force)

}
