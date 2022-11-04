Assignment 3
================
CP
2022-11-04

### API

``` r
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2%20trial%20vaccine")
  
counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]")
counts <- as.character(counts)
#stringr::str_extract(counts, "[REGEX FOR NUMBERS WITH COMMAS/DOTS]")
```

## There were 4009 paper for this search

``` r
query_ids <- GET(
  url      = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query    = list(
    db     = "pubmed",
    term   = "sars-cov-2 trial vaccine",
    retmax = 250
  )
)
```

``` r
ids <- httr::content(query_ids)
```

``` r
ids <- as.character(ids)
ids <- stringr::str_extract_all(ids, "<Id>[[:digit:]]+</Id>")[[1]]
ids <- stringr::str_remove_all(ids, "<?Id>")
ids <- stringr::str_remove_all(ids, "</")
head(ids)
```

    ## [1] "36328399" "36327352" "36322837" "36320825" "36314847" "36307830"

``` r
publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    db      = "pubmed", 
    Id      = paste(ids, collapse = ","),
    retmax  = 250, 
    rettype = "abstract" 
    )
)
publications <- httr::content(publications)
head(publications)
```

    ## $node
    ## <pointer: 0x000001ea4c2ae470>
    ## 
    ## $doc
    ## <pointer: 0x000001ea48713710>

``` r
pub_char_list <- xml2::xml_children(publications)
pub_char_list <- sapply(pub_char_list, as.character)
```
