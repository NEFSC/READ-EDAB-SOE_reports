# State of the Ecosystem Documents

This repository contains code used to create the State of the Ecosystem reports for the Mid Atlantic and New England regions, as well as the resulting report documents and presentation slides.

## 2026 report 

### Report documents

- [2026 Mid-Atlantic Council report](2026/SOE2026_MAFMC_Council.pdf) 
- [2026 Mid-Atlantic SSC report](2026/SOE2026_MAFMC_SSC.pdf) 
- [2026 New England SSC and Council report](2026/SOE2026_NEFMC_IR_Edits.pdf)

### Supporting documents

- [2026 Research Response Memo](2026/SOE_Response_Memo.pdf)
- [2026 Mid-Atlantic Cover Letter](2026/SOE2026_MAFMC_Cover_Letter.pdf)
- [2026 New England Cover Letter](2026/SOE2026_NEFMC_Cover_Letter.pdf)

### Presentation slides

- [Mid Atlantic Council slides](2026/SOE2026_MAFMC.html) 
- [Mid Atlantic SSC slides](2026/SOE2026_MAFMC_SSC.html) 
- [New England Council slides](2026/SOE2026_NEFMC_final.html) 
- [New England SSC slides](2026/SOE2026_NEFMC_SSC_final_V2.html)
- [SOE 2026 Overview Seminar](2026/parent_presentation_overview_seminar.html) 
- [CESCC slides](2026/SOE2026_newengland_CESC.html) 
- [ECCG slides](2026/SOE2026_newengland_ECCG.html) 
- [Synthesis meeting slides](2026/parent_presentation_synthesis.html) (*updated as of 1/14/2026*)

## How to update the reports

### Updating figures and captions

Figures are created in `scripts/create_plots.R` (`scripts/create_plots_slides.R` for slide plots) and saved to the `images/` folder.
Figure and caption information is read into the Rmarkdowns from the spreadsheet `utils/figure_captions_summary.csv`. Full figure filepaths do not need to be entered into this spreadsheet, the code will match the first portion of the filename to the appropriate figure in the `images/` folder. *This means that the `images/` folder should not contain duplicate images.*
That spreadsheet links the file names and captions for each report to a lookup key that is used in the Rmarkdowns to call in the appropriate figure and caption.

Steps to update figures and captions:
1. Update and run `scripts/create_plots.R` to create updated figures.
1. Delete any duplicate figures from the `images/` folder. This can be done with the function in `R/cleanup_images.R`
1. Update the `utils/figure_captions_summary.csv` spreadsheet with any new or changed figure captions.
1. If you are adding a new figure, add it to the appropriate Rmarkdown with the syntax: `return_plot("chunk-name", region = "RegionName")`, where `chunk-name` is the lookup key in the `figure_captions_summary.csv` spreadsheet and `RegionName` is either `MidAtlantic`, `NewEngland`, or `BothRegions` (for figures that appear in both reports).
1. Re-knit the report from `run_reports.R` script.

### Ensuring that htmls are archivable

Ensure that your final html is knitted with the following setting:

```
format:
  revealjs:
    embed-resources: true 
```

This will create a self-contained html file that can be archived and shared. The parent presentation qmds used in 2026 already have this feature enabled.

### PDF document checks

1. `scripts/checks` contains 5 check scripts that check for common errors in the pdf reports. Run these scripts and fix any errors before the report is submitted.

## Repo organization

- `2026/` - contains the 2026 reports, supporting documents, and presentation slides
- `R/` - contains functions used to create figures, captions, run statistics, and create supporting documentaiton.
- `archive/` - contains miscellaneous files staged for deletion.
- `child_docs/` - contains the child Rmarkdowns for each pdf report. These must be called from a parent Rmarkdown and cannot be run directly.
- `child_slides/` - contains the child Quarto docs for each presentation slide deck. These must be called from a parent Quarto and cannot be run directly.
- `images/` - contains all figures used in the reports and slides. Updated every report cycle! See archived documents for prior years' figures.
- `libs/` - contains css and js libraries used in the reports and slides.
- `scripts/` - contains R scripts to create figures and run the reports.
- `utils/` - contains utility files used in the reports and slides, including the figure caption spreadsheet. *No .R, .Rmd, or .qmd files stored here!*
- Files in the root directory
  - `SOE_Cover_Letter.Rmd` - template for the cover letter for each report
  - `SOE_Response_Memo.Rmd` - template for the research response memo for each report
  - `parent_report.Rmd` - template for the pdf reports. Calls child documents in `child_docs/`
  - `parent_presentation***.qmd` - templates for the presentation slides for various venues. Calls child slides in `child_slides/`
  - `run_reports.R` - script to generate pdf reports for both regions and for the cover letter and response memo
  - `run_slides.R` - script to generate slides for various presentations

#### Legal Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
