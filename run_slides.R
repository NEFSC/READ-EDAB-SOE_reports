## mid slides ----
now <- Sys.time()
quarto::quarto_render(
  input = here::here("parent_presentation.qmd"),
  output_file = "midatlantic.html",
  execute_params = list(
    region = "MidAtlantic",
    council = "MAFMC",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    date = Sys.Date() |> format("%B %d, %Y"),
    author = "Abigail Tyrell, lead editor, NEFSC"
  ),
  output_format = "all"
)
difftime(Sys.time(), now)
