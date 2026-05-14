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
  input = here::here("parent_presentation_council.qmd"),
  output_file = "SOE2026_NEFMC_final.html",
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
  output_file = "SOE2026_NEFMC_SSC_final_V2.html",
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

## Mid ----
now <- Sys.time()
quarto::quarto_render(
  input = here::here("parent_presentation_ssc.qmd"),
  output_file = "SOE2026_MAFMC_SSC.html",
  execute_params = list(
    region = "MidAtlantic",
    council = "MAFMC",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    date = "May 13, 2026",
    author = "Abby Tyrell, lead editor, NEFSC"
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

## Core Team Presenation ----
now <- Sys.time()
quarto::quarto_render(
  input = here::here("parent_presentation_ECCG.qmd"),
  output_file = "SOE2026_newengland_ECCG",
  execute_params = list(
    region = "both",
    council = "NEFMC",
    fig_caption = here::here("utils/figure_captions_summary.csv"),
    date = Sys.Date() |> format("%B %d, %Y"),
    author = "Joe Caracappa & Abigail Tyrell: lead editors, NEFSC"
  ),
  output_format = "all"
)
difftime(Sys.time(), now)
