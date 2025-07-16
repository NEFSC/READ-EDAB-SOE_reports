rmarkdown::render(
  input = here::here("parent_report.Rmd"),
  output_file = here::here("parent_report_midatlantic.pdf"),
  params = list(region = "MidAtlantic"),
  output_dir = here::here("output"),
  output_format = bookdown::pdf_document2(includes = rmarkdown::includes(in_header = "utils/header1_midatlantic.tex"))
)

rmarkdown::render(here::here("parent_report.Rmd"),
  output_file = here::here("parent_report_newengland.pdf"),
  params = list(region = "NewEngland"),
  output_dir = here::here("output"),
  output_format = bookdown::pdf_document2(includes = rmarkdown::includes(in_header = "utils/header1_newengland.tex"))
)
