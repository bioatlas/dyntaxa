
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dyntaxa

The goal of the `dyntaxa` R package is to provide a read-only R package
to interface with Dyntaxa - the taxonomic database of organisms in
Sweden.

Dyntaxa contains information about 61,500 species occurring in Sweden.
This includes about 95% of known multicellular species – remaining gaps
mainly found among the fungi. The scope of organisms include
multicellular species documented in Sweden and such unicellular species
that are included in environmental monitoring by the Swedish EPA. In
addition to these species there are many names at other taxonomic
levels, scientific synonyms, Swedish vernaculars.

## Credits and acknowledgements

The Dyntaxa database is published by
[Artdatabanken](https://www.gbif.org/publisher/b8323864-602a-4a7d-9127-bb903054e97d)
at
[GBIF.org](https://www.gbif.org/dataset/de8934f4-a136-481c-a87a-b0b202b80a31)
by Johan Liljeblad.

## Citation to use for refering to Dyntaxa

Liljeblad J (2019). Dyntaxa. Svensk taxonomisk databas. ArtDatabanken.
Checklist dataset <https://doi.org/10.15468/j43wfc>.

## Installation

You can install the `dyntaxa` R package from
[GitHub](https://github.com/bioatlas/dyntaxa) with:

``` r
library(devtools)
install_github("bioatlas/dyntaxa")
```

## Example usage

This package can be used to automate the following tasks:

  - Taxonomic identifier from a taxonomic name and vice versa
  - Taxonomic name from a vernacular (common) name and vice versa
  - Taxonomic hierarchy/classification from identifier or name
  - Taxonomic children of an identifier or name
  - All taxa downstream to a certain rank from identifier or name
  - Taxonomic synonyms from identifier or name

Here are some short and simple usage examples which shows you how to
download and access data from Dyntaxa for those tasks.

``` r

# we use dplyr for data manipulation (pipe, filtering etc)
suppressPackageStartupMessages(library(dplyr))

library(dyntaxa)
    __             __
.--|  .--.--.-----|  |_.---.-.--.--.---.-.
|  _  |  |  |     |   _|  _  |_   _|  _  |
|_____|___  |__|__|____|___._|__.__|___._|
      |_____|

# looking up identifiers/keys from taxon names and vice versa

# taxonomic name from taxonomic identifier
dyntaxa_name_from_id(5000001)
[1] "Animalia"

# taxonomic identifier from taxonomic name
key <- dyntaxa_id_from_name("Alces alces")
key
[1] "206046"

# the taxon key or identifier is often needed to retrieve taxonomic data

# taxonomic hierarchy/classification from identifier or name
dyntaxa_classification(key)
# A tibble: 13 x 5
   taxonId scientificName parentNameUsageID taxonRank   rankDistance
   <chr>   <chr>          <chr>             <chr>              <dbl>
 1 206046  Alces alces    1001660           species                0
 2 1001660 Alces          6007818           genus                  1
 3 6007818 Capreolinae    2002172           subfamily              2
 4 2002172 Cervidae       2002171           family                 3
 5 2002171 Ruminantia     3000305           suborder               4
 6 3000305 Artiodactyla   6034822           order                  5
 7 6034822 Laurasiatheria 6034827           superorder             6
 8 6034824 Placentalia    4000107           infraclass             7
 9 4000107 Mammalia       6034835           class                  8
10 6000993 Vertebrata     4000099           infraphylum            9
11 4000099 Craniata       5000034           subphylum             10
12 5000034 Chordata       5000001           phylum                11
13 5000001 Animalia       <NA>              kingdom               12

# taxa downstream from identifier or name
dyntaxa_downstream(dyntaxa_id_from_name("Cervidae"))
# A tibble: 26 x 5
   taxonId scientificName parentNameUsageID taxonRank rankDistance
   <chr>   <chr>          <chr>             <chr>            <dbl>
 1 2002172 Cervidae       2002171           family               0
 2 6007818 Capreolinae    2002172           subfamily            1
 3 6007819 Cervinae       2002172           subfamily            1
 4 6011558 Hydropotinae   2002172           subfamily            1
 5 1001659 Capreolus      6007818           genus                2
 6 1001660 Alces          6007818           genus                2
 7 1001661 Rangifer       6007818           genus                2
 8 1015254 Odocoileus     6007818           genus                2
 9 1001657 Cervus         6007819           genus                2
10 1001658 Dama           6007819           genus                2
# … with 16 more rows

# taxonomic immediate children of an identifier or name
dyntaxa_children(dyntaxa_id_from_name("Carnivora"))
# A tibble: 2 x 5
  taxonId scientificName parentNameUsageID taxonRank rankDistance
  <chr>   <chr>          <chr>             <chr>            <dbl>
1 2002151 Feliformia     3000303           suborder             1
2 2002153 Caniformia     3000303           suborder             1

# taxa downstream of a specific taxon identifier 
# can be filtered at species level (or at other ranks)
dyntaxa_downstream(dyntaxa_id_from_name("Carnivora")) %>% 
  filter(taxonRank == "species")
# A tibble: 26 x 5
   taxonId scientificName           parentNameUsage… taxonRank rankDistance
   <chr>   <chr>                    <chr>            <chr>            <dbl>
 1 100057  Lynx lynx                1001652          species              4
 2 233620  Felis catus              1009403          species              4
 3 6007816 Urva javanica            6037429          species              4
 4 267320  Canis lupus              1001643          species              4
 5 6011549 Canis aureus             1001643          species              4
 6 100005  Vulpes lagopus           1001644          species              4
 7 206026  Vulpes vulpes            1001644          species              4
 8 206028  Nyctereutes procyonoides 1001646          species              4
 9 100145  Ursus arctos             1001642          species              4
10 206029  Mustela erminea          1001647          species              4
# … with 16 more rows

# search a fulltext index of Dyntaxa a vernacular name
dyntaxa_search_all("blåklocka") %>% 
  select(taxonId, scientificName, vernacularName, title, creator)
# A tibble: 6 x 5
  taxonId scientificName        vernacularName   title           creator   
  <chr>   <chr>                 <chr>            <chr>           <chr>     
1 220018  Campanula persicifol… stor blåklocka   Thomas Karlsso… Aronsson,…
2 220023  Campanula rotundifol… liten blåklocka  Thomas Karlsso… Aronsson,…
3 235014  Campanula americana   amerikansk blåk… Databas levere… Svenska k…
4 223657  Campanula rotundifol… Blåklocka        Thomas Karlsso… Aronsson,…
5 223657  Campanula rotundifol… liten blåklocka  Thomas Karlsso… Aronsson,…
6 265920  Campanula persicifol… vanlig stor blå… Databas levere… Svenska k…

# another example of a full text search 
dyntaxa_search_all("Thomas Karlssons Kärlväxtlista") %>%
  filter(taxonRank == "family")
# A tibble: 15 x 10
   taxonId scientificName taxonRank scientificNameA… taxonomicStatus
   <chr>   <chr>          <chr>     <chr>            <chr>          
 1 2002896 Menispermaceae family    Juss.            accepted       
 2 2002886 Ixioliriaceae  family    Nakai            accepted       
 3 2002887 Acoraceae      family    Martinov         accepted       
 4 2002889 Bignoniaceae   family    Juss.            accepted       
 5 2002890 Cercidiphylla… family    Engl.            accepted       
 6 2002892 Hymenophyllac… family    Mart.            accepted       
 7 2002895 Tofieldiaceae  family    Takht.           accepted       
 8 2002897 Myrtaceae      family    Juss.            accepted       
 9 2002898 Piperaceae     family    Giseke           accepted       
10 2002899 Ruppiaceae     family    Horan.           accepted       
11 2002921 Cannaceae      family    Juss.            accepted       
12 2002922 Musaceae       family    Juss.            accepted       
13 2002900 Styracaceae    family    DC. & Spreng.    accepted       
14 2002923 Platanaceae    family    T. Lestib.       accepted       
15 2002894 Nartheciaceae  family    Fr. ex Bjurzon   accepted       
# … with 5 more variables: nomenclaturalStatus <chr>,
#   vernacularName <chr>, language <chr>, title <chr>, creator <chr>

# what are synonyms for Sagedia zonata?
dyntaxa_synonyms(dyntaxa_id_from_name("Sagedia zonata"))
# A tibble: 32 x 3
   scientificName         taxonomicStatus nomenclaturalStatus
   <chr>                  <chr>           <chr>              
 1 Aspicilia curvabilis   synonym         valid              
 2 Aspicilia haerjedalica synonym         valid              
 3 Aspicilia inconspicua  synonym         valid              
 4 Aspicilia litorea      synonym         valid              
 5 Aspicilia malmeana     synonym         valid              
 6 Aspicilia obscurascens synonym         valid              
 7 Aspicilia obscurata    synonym         valid              
 8 Aspicilia pleiocarpa   synonym         valid              
 9 Aspicilia rolleana     synonym         valid              
10 Aspicilia subarctica   synonym         valid              
# … with 22 more rows

# more examples for synonyms
dyntaxa_search_all("scientificName:Phyllachora graminis")
# A tibble: 3 x 10
  taxonId scientificName taxonRank scientificNameA… taxonomicStatus
  <chr>   <chr>          <chr>     <chr>            <chr>          
1 5509    Phyllachora g… species   (Pers.:Fr.) Fuc… accepted       
2 urn:ls… Phyllachora g… species   Rehm             synonym        
3 urn:ls… Phyllachora g… species   (Fr.) Sacc.      synonym        
# … with 5 more variables: nomenclaturalStatus <chr>,
#   vernacularName <chr>, language <chr>, title <chr>, creator <chr>
dyntaxa_synonyms(dyntaxa_id_from_name("Phyllachora graminis"))
# A tibble: 21 x 3
   scientificName                  taxonomicStatus nomenclaturalStatus
   <chr>                           <chr>           <chr>              
 1 Sphaeria graminis               synonym         valid              
 2 Sphaeria graminis var. elymorum synonym         valid              
 3 Sphaeria graminis var. poarum   synonym         valid              
 4 Scirrhia poae                   synonym         valid              
 5 Polystigma graminis             synonym         valid              
 6 Phyllachora poae-pratensis      synonym         valid              
 7 Phyllachora poae                synonym         valid              
 8 Phyllachora poae f. alpina      synonym         valid              
 9 Phyllachora melicae             synonym         valid              
10 Phyllachora hordei              synonym         valid              
# … with 11 more rows

# does "Citronfjäril" / "Gonepteryx rhamni" have synonyms?
key <- dyntaxa_search_vernacular("citronfjäril")$taxonId
dyntaxa_name_from_id(key)
[1] "Gonepteryx rhamni"
dyntaxa_synonyms(key)
# A tibble: 1 x 3
  scientificName taxonomicStatus nomenclaturalStatus
  <chr>          <chr>           <chr>              
1 Papilio rhamni synonym         valid              
```

## Other use cases

The vignette provides more usage information on other common use cases,
such as:

  - Using the above functions with vectors of several keys/identifiers
    or names
  - Resolving potentially misspelled scientific names
  - Exporting data
  - Looking at counts / aggregates
