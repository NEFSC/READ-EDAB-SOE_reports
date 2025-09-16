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
      in_header = here::here("utils/header1_midatlantic.tex")
      # TODO: create different header for draft doc?
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
  output_file = "midatlantic.pdf",
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

## mid report (html) ----

rmarkdown::render(
  input = here::here("parent_report.Rmd"),
  # output_file = "midatlantic.pdf",
  output_file = "midatlantic.html",
  params = list(
    region = "MidAtlantic",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    ## TODO: caching saves as parent file name, could cause problems with regions
    cache = TRUE
  ),
  output_format = bookdown::html_document2()
)

## NE report (pdf) ----
now <- Sys.time()
rmarkdown::render(
  here::here("parent_report.Rmd"),
  output_file = here::here("newengland.pdf"),
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

## NE report (docx) ----
### don't render to word like this, convert the pdf

rmarkdown::render(
  here::here("parent_report.Rmd"),
  output_file = here::here("newengland.docx"),
  params = list(
    region = "NewEngland",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    cache = TRUE,
    id_child_docs = TRUE
  ),
  # intermediates_dir = here::here("output"),
  # knit_root_dir = here::here("output"),
  # output_dir = here::here("output"),
  # output_format = bookdown::word_document2(
  #   # includes = rmarkdown::includes(
  #   #   in_header = here::here("utils/header1_newengland.tex")
  #   # ),
  #   # keep_tex = TRUE,
  #   # toc = FALSE,
  #   # number_sections = FALSE
  # )
)
