# full report ----

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
  output_file = "SOE2026_MAFMC_Council.pdf",
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
    number_sections = FALSE,
    # don't think this is doing anything?
    extra_dependencies = "float"
  )
)
difftime(Sys.time(), now)

## NE report (pdf) ----
now <- Sys.time()
rmarkdown::render(
  here::here("parent_report.Rmd"),
  output_file = here::here("SOE2026_NEFMC_final.pdf"),
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

#Render CoverLetter ----
### NE ----
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

### mid ----
rmarkdown::render(
  input = here::here("SOE_Cover_Letter.Rmd"),
  output_file = here::here("SOE2026_MAFMC_Cover_Letter.pdf"),
  params = list(
    region = "MidAtlantic",
    signature = here::here("utils/signature_tyrell.jpg"),
    cache = FALSE,
    id_child_docs = TRUE
  )
)
