## both reports, for text edits (pdf) ----
now <- Sys.time()
rmarkdown::render(
  input = here::here("parent_report.Rmd"),
  output_file = "bothregions.pdf",
  params = list(
    region = "MidAtlantic",
    # TODO: have to pass a region, working on overriding in child docs and figures
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    ## TODO: caching saves as parent file name, could cause problems with regions
    cache = FALSE,
    id_child_docs = TRUE,
    draft = TRUE
  ),
  output_format = bookdown::pdf_document2(
    includes = rmarkdown::includes(
      in_header = here::here("utils/header1_bothreports.tex")
    ),
    keep_tex = TRUE,
    toc = FALSE,
    number_sections = FALSE
  )
)
difftime(Sys.time(), now)


## mid report (pdf) ----
now <- Sys.time()
rmarkdown::render(
  input = here::here("parent_report.Rmd"),
  output_file = "SOE2026_MAFMC_IR_Final.pdf",
  params = list(
    region = "MidAtlantic",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    ## TODO: caching saves as parent file name, could cause problems with regions
    cache = FALSE,
    id_child_docs = TRUE,
    draft = FALSE
  ),
  output_format = bookdown::pdf_document2(
    includes = rmarkdown::includes(
      in_header = here::here("utils/header1_midatlantic.tex")
    ),
    keep_tex = TRUE,
    toc = FALSE,
    number_sections = FALSE
  )
)
difftime(Sys.time(), now)

## NE report (pdf) ----
now <- Sys.time()
rmarkdown::render(
  here::here("parent_report.Rmd"),
<<<<<<< HEAD
  output_file = here::here("SOE2026_NEFMC_IR_Edits.pdf"),
=======
  output_file = here::here("SOE2026_NEFMC_V2.pdf"),
>>>>>>> origin/edits_AT
  params = list(
    region = "NewEngland",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    cache = FALSE,
    id_child_docs = TRUE
  ),
  output_format = bookdown::pdf_document2(
    includes = rmarkdown::includes(
      in_header = here::here("utils/header1_newengland.tex")
    ),
    keep_tex = TRUE,
    toc = FALSE,
    number_sections = FALSE
  )
)
difftime(Sys.time(), now)

#Render CoverLetter
rmarkdown::render(
  input = here::here("SOE_Cover_Letter.Rmd"),
  output_file = here::here("SOE2026_NEFMC_Cover_Letter.pdf"),
  params = list(
    region = "NewEngland",
    signature = here::here("utils/caracappa_signature.pdf"),
    cache = FALSE,
    id_child_docs = TRUE
  )
)
