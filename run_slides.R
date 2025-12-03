## mid slides ----
now <- Sys.time()
quarto::quarto_render(
  input = here::here("parent_presentation.qmd"),
  output_file = "midatlantic.html",
  execute_params = list(
    region = "MidAtlantic",
    council = "MAFMC",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    date = Sys.Date(),
    author = "Abigail Tyrell, lead editor, NEFSC"
  ),
  output_format = "html"
)
difftime(Sys.time(), now)
