rmarkdown::render(
  input = here::here("parent_report.Rmd"),
  output_file = here::here("output/parent_report_midatlantic.pdf"),
  params = list(
    region = "MidAtlantic",
    fig_caption = here::here("utils/fig-captions.csv")
  ),
  intermediates_dir = here::here("output"),
  knit_root_dir = here::here("output"),
  output_dir = here::here("output"),
  output_format = # c(
    bookdown::pdf_document2(
      includes = rmarkdown::includes(in_header = here::here("utils/header1_midatlantic.tex")),
      keep_tex = TRUE,
      toc = FALSE,
      number_sections = FALSE
    ) # ,
  # bookdown::html_document2(df_print = "paged")#,
  # bookdown::word_document2(toc = FALSE)
  # )
)

rmarkdown::render(here::here("parent_report.Rmd"),
  output_file = here::here("parent_report_newengland.pdf"),
  params = list(region = "NewEngland"),
  intermediates_dir = here::here("output"),
  knit_root_dir = here::here("output"),
  output_dir = here::here("output"),
  output_format = bookdown::pdf_document2(
    includes = rmarkdown::includes(in_header = here::here("utils/header1_newengland.tex")),
    keep_tex = TRUE,
    toc = FALSE,
    number_sections = FALSE
  )
)
