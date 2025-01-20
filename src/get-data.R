#===============================================================================
# 2025-01-18 -- mdpi-pop
# task
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)

library(httr2)
library(rvest)


raw_page <- read_html("https://www.mdpi.com/journal/populations/editors")


# get the names of the 30 editors -----------------------------------------

eds <- raw_page |>
    html_elements(".generic-item.editor-div") |>
    html_text(trim = TRUE) |>
    tibble(raw = _) |>
    transmute(
        name = raw  |>
            str_extract("\\s.*\\s") |>
            str_remove("             ") |>
            str_remove(" Dr.") |>
            str_remove("\n"),
        prof = raw  |> str_detect("Prof."),
        ed_si = raw  |> str_detect("Special Issues")
    )


eds |> skimr::skim()


# get editors profile photos ----------------------------------------------

img <- raw_page |>
    html_elements(".sciprofiles-link__image") |>
    html_attr("src") |>
    tibble(foo = _) |>
    distinct() |>
    transmute(
        img = paste0("https://www.mdpi.com", foo) |>
            str_remove("\\?.*$")
    )



save(eds, img, file = "out/eds-names.rda")


library(rscopus)
set_api_key(Sys.getenv("scopus_api"))

query1 <- 'TITLE-ABS-KEY ( ( recreation ) AND ( management ) AND (challenge)'
run1 <- scopus_search(
    query = query1,
    api_key = Sys.getenv("scopus_api"),
    count = 20,
    view = c('COMPLETE'),
    start = 0,
    verbose = TRUE,
    max_count = 20000,
    http = 'https://api.elsevier.com/content/search/scopus',
    headers = inst_token_header(insttoken),
    wait_time = 0
)
