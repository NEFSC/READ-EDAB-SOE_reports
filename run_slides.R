# Council slides ----

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

## ne slides ----
now <- Sys.time()
quarto::quarto_render(
  input = here::here("parent_presentation.qmd"),
  output_file = "SOE2026_NEFMC_SSC_present.html",
  execute_params = list(
    region = "NewEngland",
    council = "NEFMC",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    date = Sys.Date() |> format("%B %d, %Y"),
    author = "Joe Caracappa, lead editor, NEFSC"
  ),
  output_format = "all"
)
difftime(Sys.time(), now)

## all slides ----
now <- Sys.time()
quarto::quarto_render(
  input = here::here("parent_presentation.qmd"),
  execute_params = list(
    region = "Both",
    council = "NEFMC, MAFMC",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    date = Sys.Date() |> format("%B %d, %Y"),
    author = "NEFSC"
  ),
  output_format = "all"
)
difftime(Sys.time(), now)

# SSC slides ----

## NE ----
now <- Sys.time()
quarto::quarto_render(
  input = here::here("parent_presentation_ssc.qmd"),
  output_file = "newengland_ssc_draft_v2.html",
  execute_params = list(
    region = "NewEngland",
    council = "NEFMC",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    date = Sys.Date() |> format("%B %d, %Y"),
    author = "Joe Caracappa, lead editor, NEFSC"
  ),
  output_format = "all"
)
difftime(Sys.time(), now)

## NEFMC CESC ----
now <- Sys.time()
quarto::quarto_render(
  input = here::here("parent_presentation_CESC.qmd"),
  output_file = "SOE2026_newengland_CESC",
  execute_params = list(
    region = "NewEngland",
    council = "NEFMC",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    date = Sys.Date() |> format("%B %d, %Y"),
    author = "Joe Caracappa, lead editor, NEFSC"
  ),
  output_format = "all"
)
difftime(Sys.time(), now)