# Load necessary libraries
library(stringr)
library(here)

# 1. The raw input string
raw_text <- "Sydney Alhale (SEFSC), Andrew Applegate (NEFMC), Christina Asante, Kimberly Bastille, Heather Baertlein (NMFS Atlantic HMS Management Division), Aaron Beaver (Anchor QEA), Andy Beet, Brandon Beltz, Kristan Blackhart, Ruth Boettcher (Virginia Department of Game and Inland Fisheries), Mandy Bromilow (NOAA Chesapeake Bay Office), Joseph Caracappa, Samuel Chavez-Rosales, Baoshan Chen (Stony Brook University), Zhuomin Chen (UConn), Doug Christel (GARFO), Patricia Clay, Lisa Colburn, Jennifer Cudney (NMFS Atlantic HMS Management Division), Tobey Curtis (NMFS Atlantic HMS Management Division), Cameron Day, Kiley Dancy (MAFMC), Art Degaetano (Cornell U), Geret DePiper, Bart DiFiore (MA DMF), Gregory Ellis, Emily Farr (NMFS Office of Habitat Conservation), Michael Fogarty, Paula Fratantoni, Kevin Friedland, Marjy Friedrichs (VIMS), Sarah Gaichas (Hydra Scientific), Ben Galuardi (GAFRO), Avijit Gangopadhyay (School for Marine Science and Technology, University of Massachusetts Dartmouth), James Gartland (VIMS), Lori Garzio (Rutgers University), Glen Gawarkiewicz (WHOI), Maxwell Grezlik, Laura Gruenburg (Stony Brook University), Sean Hardison, Amanda Hart, Dvora Hart, Cliff Hutt (NMFS Atlantic HMS Management Division), Kimberly Hyde, Grace Jensen (WHOI), Toni Kerns (ASMFC), John Kocik, Steve Kress (National Audubon Society’s Seabird Restoration Program), Young-Oh Kwon (Woods Hole Oceanographic Institution), Scott Large, Gabe Larouche (Cornell U), Daniel Linden, Andrew Lipsky, Sean Lucey (@ Orchard), Don Lyons (National Audubon Society’s Seabird Restoration Program), Kevin Madley, George Maynard, Chris Melrose, Anna Mercer, Shannon Meseck, Kiera Morrill, Ryan Morse, Ray Mroch (SEFSC), Nicole Mucci, Brandon Muffley (MAFMC), Robert Murphy, Kimberly Murray, NEFSC staff, David Moe Nelson (NCCOS), Chris Orphanides, Stephanie Owen, Richard Pace, Debi Palka, Tom Parham (Maryland DNR), CJ Pellerin (NOAA Chesapeake Bay Office), Charles Perretti, Kristin Precoda, Grace Roskar (NMFS Office of Habitat Conservation), Andrew Ross, Jeffrey Runge (U Maine), Grace Saba (Rutgers University), Vincent Saba, Sarah Salois, Chris Schillaci (GARFO), Amy Schueller (SEFSC), Teresa Schwemmer (URI), Tarsila Seara, Dave Secor (CBL), Emily Slesinger, Angela Silva, Adrienne Silver (WHOI), Laurel Smith, Linus Stoltz (Commercial Fisheries Research Foundation), Talya tenBrink (GARFO), Abigail Tyrell, Rebecca Van Hoeck, Bruce Vogt (NOAA Chesapeake Bay Office), Ron Vogel (University of Maryland Cooperative Institute for Satellite Earth System Studies and NOAA/NESDIS Center for Satellite Applications and Research), John Walden, Harvey Walsh, Sarah Weisberg, Changhua Weng, Dave Wilcox (VIMS), Timothy White (Environmental Studies Program, BOEM), Sarah Wilkin (NMFS Office of Protected Resources), Mark Wuenschel, Joseph Warren, Zhitao Yu, Qian Zhang (U Maryland)"

# 2. Split the string by commas that are NOT inside parentheses
entries <- str_split(raw_text, ",\\s*(?![^()]*\\))")[[1]]

# 3. Process each entry to extract First Name, Last Name, and Affiliation
process_entry <- function(entry) {
  entry <- str_trim(entry)
  
  # Regex to capture content before parentheses and content inside parentheses
  match <- str_match(entry, "^(.*?)(?:\\s*\\((.*)\\))?$")
  
  full_name <- str_trim(match[2])
  affiliation <- str_trim(match[3])
  
  # Split First and Last components
  # word(x, 1) gets the first word; word(x, 2, -1) gets the rest
  first_name <- word(full_name, 1)
  last_name <- word(full_name, 2, -1)
  
  # Handle cases where there is no last name (e.g. "NEFSC staff")
  if (is.na(last_name)) {
    last_name <- ""
  }
  
  # Default affiliation to NEFSC if missing
  if (is.na(affiliation) || affiliation == "") {
    affiliation <- "NEFSC"
  }
  
  return(c(First_Name = first_name, Last_Name = last_name, Affiliation = affiliation))
}

# 4. Create the data frame
contributor_df <- as.data.frame(t(sapply(entries, process_entry)), stringsAsFactors = FALSE) |> 
  dplyr::arrange(Last_Name,desc = T)
rownames(contributor_df) <- NULL


# 5. Export to CSV
write.csv(contributor_df, here::here("utils", "contributors_list.csv"), row.names = FALSE)

# Print a preview
print(head(contributor_df))
cat("\nTotal contributors parsed:", nrow(contributor_df))
