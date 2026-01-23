# State of the Ecosystem Documents

This repository contains code used to create the State of the Ecosystem reports for the Mid Atlantic and New England regions, as well as the resulting report documents and presentation slides.

## 2026 report -- **drafting in progress**

These documents are in progress and subject to change. Please check back later for the final published versions.

### Report documents

- [Mid Atlantic](midatlantic.pdf) (*in progress -- has not been updated with 2026 data*)
- [New England](newengland.pdf) (*in progress -- has not been updated with 2026 data*)
- [Combined report](bothregions.pdf) (to assist with editing only, will not be published as a final report) (*in progress -- has not been updated with 2026 data*)

### Presentation slides

- [Mid Atlantic](midatlantic.html) (*in progress -- has not been updated with 2026 data*)
- [New England](newengland.html) (*in progress -- has not been updated with 2026 data*)
- [Combined report](parent_presentation.html) (to assist with editing only, will not be presented as a final report) (*in progress -- has not been updated with 2026 data*)
- [Synthesis meeting slides](parent_presentation_synthesis.html) (*updated as of 1/14/2026*)

## How to update the reports

### Updating figures and captions

Figures are created in `scripts/create_plots.R` (`scripts/create_plots_slides.R` for slide plots) and saved to the `images/` folder.
Figure and caption information is read into the Rmarkdowns from the spreadsheet `utils/figure_captions_summary.csv`. Full figure filepaths do not need to be entered into this spreadsheet, the code will match the first portion of the filename to the appropriate figure in the `images/` folder. *This means that the `images/` folder should not contain duplicate images.*
That spreadsheet links the file names and captions for each report to a lookup key that is used in the Rmarkdowns to call in the appropriate figure and caption.

Steps to update figures and captions:
1. Update and run `scripts/create_plots.R` to create updated figures.
1. Delete any duplicate figures from the `images/` folder.
1. Update the `utils/figure_captions_summary.csv` spreadsheet with any new or changed figure captions.
1. If you are adding a new figure, add it to the appropriate Rmarkdown with the syntax: `return_plot("chunk-name", region = "RegionName")`, where `chunk-name` is the lookup key in the `figure_captions_summary.csv` spreadsheet and `RegionName` is either `MidAtlantic`, `NewEngland`, or `BothRegions` (for figures that appear in both reports).
1. Re-knit the report from `run_reports.R` script.

#### Legal Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
