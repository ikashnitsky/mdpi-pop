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
        id = 1:30 |> paste() |> str_pad(2, "left", pad = "0"),
        id_sur = name |>
            word(-1) |>
            str_to_lower() |>
            stringi::stri_trans_general('Latin-ASCII'),
        is_prof = raw  |> str_detect("Prof."),
        is_ed_si = raw  |> str_detect("Special Issues")
    )



eds |> skimr::skim()


# get editors profile photos ----------------------------------------------

img <- raw_page |>
    html_elements(".sciprofiles-link__image") |>
    html_attr("src") |>
    tibble(foo = _) |>
    distinct() |>
    transmute(
        id = 1:30 |> paste() |> str_pad(2, "left", pad = "0"),
        img = paste0("https://www.mdpi.com", foo) |>
            str_remove("\\?.*$")
    )



save(eds, img, file = "out/eds-names.rda")


# library(rscopus)
# set_api_key(Sys.getenv("scopus_api"))
#
# query1 <- 'TITLE-ABS-KEY ( ( recreation ) AND ( management ) AND (challenge)'
# run1 <- scopus_search(
#     query = query1,
#     api_key = Sys.getenv("scopus_api"),
#     count = 20,
#     view = c('COMPLETE'),
#     start = 0,
#     verbose = TRUE,
#     max_count = 20000,
#     http = 'https://api.elsevier.com/content/search/scopus',
#     headers = inst_token_header(insttoken),
#     wait_time = 0
# )



# get special issues DOIs -------------------------------------------------
#
# # Define the regex pattern for DOIs
# doi_pattern <- "https://doi\\.org/\\d{2}\\.\\d{4,9}/[\\w.\\-]+"

si_dois <- function(url, id_sur, id_si = "01") {
    read_html(url) |>
        html_elements(".article-content") |>
        html_text(trim = TRUE) |>
        str_extract("https://doi\\.org/\\d{2}\\.\\d{4,9}/[\\w.\\-]+") |>
        tibble(
            id_sur,
            id_si,
            doi = _
        )
}

# 01 lopez-carr

si0101 <- si_dois(
    "https://www.mdpi.com/journal/sensors/special_issues/landcover_mapping",
    id_sur = "lopez-carr",
    id_si = "01"
)

si0102 <- si_dois(
    "https://www.mdpi.com/journal/sensors/special_issues/30F63V9950",
    id_sur = "lopez-carr",
    id_si = "02"
)

# 12 kelly

si1201 <- si_dois(
    "https://www.mdpi.com/journal/ijerph/special_issues/advance_worker",
    id_sur = "kelly",
    id_si = "01"
)

si1202 <- si_dois(
    "https://www.mdpi.com/journal/jcm/special_issues/Positional_Cranial_Deformation",
    id_sur = "kelly",
    id_si = "02"
)

si1203 <- si_dois(
    "https://www.mdpi.com/journal/ijerph/special_issues/Environmental_Exposures_Occupation",
    id_sur = "kelly",
    id_si = "03"
)

si1204 <- si_dois(
    "https://www.mdpi.com/journal/humans/special_issues/defining_humans",
    id_sur = "kelly",
    id_si = "04"
)


# 13 kim

si1301 <- si_dois(
    "https://www.mdpi.com/journal/genes/special_issues/1HPVYDA1CY",
    id_sur = "kim",
    id_si = "01"
)

# 14 land

si1401 <- si_dois(
    "https://www.mdpi.com/journal/ijerph/special_issues/7WV9Y6DBO6",
    id_sur = "land",
    id_si = "01"
)

# 15 lao

si1501 <- si_dois(
    "https://www.mdpi.com/journal/genes/special_issues/Evolutionary_Medicine",
    id_sur = "lao",
    id_si = "01"
)

# 20 paul

si2001 <- si_dois(
    "https://www.mdpi.com/journal/water/special_issues/Hydrology_Public_Health",
    id_sur = "paul",
    id_si = "01"
)


# 21 peters

si2101 <- si_dois(
    "https://www.mdpi.com/journal/ijerph/special_issues/rural_disparities",
    id_sur = "peters",
    id_si = "01"
)

si2102 <- si_dois(
    "https://www.mdpi.com/journal/healthcare/special_issues/X9X93I4313",
    id_sur = "peters",
    id_si = "02"
)

# 22 radisauskas

si2201 <- si_dois(
    "https://www.mdpi.com/journal/applsci/special_issues/Enviromental_Variables_Stroke",
    id_sur = "radisauskas",
    id_si = "01"
)

si2202 <- si_dois(
    "https://www.mdpi.com/journal/atmosphere/special_issues/Hot_and_Cold_Spells",
    id_sur = "radisauskas",
    id_si = "02"
)

si2203 <- si_dois(
    "https://www.mdpi.com/journal/nutrients/special_issues/46TTA4272L",
    id_sur = "radisauskas",
    id_si = "03"
)

# 23 rocha

si2301 <- si_dois(
    "https://www.mdpi.com/journal/algorithms/special_issues/supervised_image_classification",
    id_sur = "rocha",
    id_si = "01"
)

si2302 <- si_dois(
    "https://www.mdpi.com/journal/smartcities/special_issues/Smart_Cities_IOT",
    id_sur = "rocha",
    id_si = "02"
)

si2303 <- si_dois(
    "https://www.mdpi.com/journal/sustainability/special_issues/Modelling_Smart_Sustainable_Cities",
    id_sur = "rocha",
    id_si = "03"
)

si2304 <- si_dois(
    "https://www.mdpi.com/journal/applsci/special_issues/Solar_Applications",
    id_sur = "rocha",
    id_si = "04"
)

si2305 <- si_dois(
    "https://www.mdpi.com/journal/entropy/special_issues/ITDSP",
    id_sur = "rocha",
    id_si = "05"
)

si2306 <- si_dois(
    "https://www.mdpi.com/journal/sustainability/special_issues/Solar_Potential_Urban_Environment",
    id_sur = "rocha",
    id_si = "06"
)

si2307 <- si_dois(
    "https://www.mdpi.com/journal/remotesensing/special_issues/multifaceted_cascading_disaster",
    id_sur = "rocha",
    id_si = "07"
)

si2308 <- si_dois(
    "https://www.mdpi.com/topics/2I5T0OPA28",
    id_sur = "rocha",
    id_si = "08"
)



# 24 rosenberg

si2401 <- si_dois(
    "https://www.mdpi.com/journal/sustainability/special_issues/challenges_and_responses_to_population_health_and_urbanization",
    id_sur = "rosenberg",
    id_si = "01"
)

si2402 <- si_dois(
    "https://www.mdpi.com/journal/ijerph/special_issues/geographic_disparities",
    id_sur = "rosenberg",
    id_si = "02"
)

si2403 <- si_dois(
    "https://www.mdpi.com/journal/sustainability/special_issues/aging_healthcare_inequalities",
    id_sur = "rosenberg",
    id_si = "03"
)

# 26 sleszynski

si2601 <- si_dois(
    "https://www.mdpi.com/journal/land/special_issues/spatial_chaos",
    id_sur = "sleszynski",
    id_si = "01"
)

si2602 <- si_dois(
    "https://www.mdpi.com/journal/land/special_issues/Spatial_Policy",
    id_sur = "sleszynski",
    id_si = "02"
)

# 30 yang

si3001 <- si_dois(
    "https://www.mdpi.com/journal/genealogy/special_issues/transnationalism",
    id_sur = "yang",
    id_si = "01"
)

si3002 <- si_dois(
    "https://www.mdpi.com/journal/genealogy/special_issues/24PZUM4CO9",
    id_sur = "yang",
    id_si = "02"
)


all_si_papers_doi <- bind_rows(
    mget(ls(pattern = "si+\\d"))
)

save(all_si_papers_doi, file = "out/all_si_papers_doi.rda")




# try to fetch crossref metadata ------------------------------------------


# https://stackoverflow.com/a/72526861/4638884
library(rcrossref)

all_si_cref <- all_si_papers_doi %>%
    select(doi) |>
    pmap(function(doi){
        cr_works(dois = doi) # returns CrossRef metadata for each doi
    }) %>%
    map(pluck("data")) %>% # cr_works returns a list, select only the 'data'
    bind_rows() |>
    bind_cols(all_si_papers_doi |> select(-doi))

save(all_si_cref, file = "out/all_si_cref.rda")
