# State of the Ecosystem Documents

This repository contains code used to create the State of the Ecosystem reports for the Mid Atlantic and New England regions, as well as the resulting report documents and presentation slides.

## 2026 report -- **drafting in progress**

These documents are in progress and subject to change. Please check back later for the final published versions.

### Report documents

- [2026 Mid Atlantic Council report](SOE2026_MAFMC_Council.pdf) 
- [2026 Mid Atlantic SSC report](SOE2026_MAFMC_SSC.pdf) 
- [2026 New England SSC and Council report](SOE2026_NEFMC_IR_Edits.pdf)
- [2026 Combined report](bothregions.pdf) (to assist with editing only, will not be published as a final report) 

### Presentation slides

- [Mid Atlantic Council slides](SOE2026_MAFMC.html) 
- [Mid Atlantic SSC slides](SOE2026_MAFMC_SSC.html) 
- [New England Council slides](SOE2026_NEFMC_final.html) 
- [New England SSC slides](SOE2026_NEFMC_SSC_final_V2.html)
- [SOE 2026 Overview Seminar](parent_presentation_overview_seminar.html) 
- [CESCC slides](SOE2026_newengland_CESC.html) 
- [ECCG slides](SOE2026_newengland_ECCG.html) 
- [Combined report](parent_presentation.html) (to assist with editing only, will not be presented as a final report)
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
