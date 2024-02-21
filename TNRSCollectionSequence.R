---
title: "Reorganizating Fern Collections with PPG I & TNRS "
author: "Joseph Kleinkopf & Harpo Faust & Lizzie Lombardi"
date: "the eternal 2023"
output: html_document
---
  
######## load packages ########
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(base)
library(TNRS)

######## summary: ###########
# 1. load the UNM database
# 2. clean up the taxonomic names and authorities
# 3. run TNRS to retrieve accepted names for the taxa in the UNM database
# 4. look over initial run results
# 5. Run TNRS for unresolved taxa names (no opinoin, no submit, and na)
# 6. re-run TNRS using wfo for taxa that had no good matches, or were not submitted
# 7. return taxon names that were not submitted using TNRS wfo (no_submit2)
# 8. return taxon names for which there were no good matches using wfo (no_opinion2)
# 9. combine tnrs runs into one data frame. 
# 10. combine the database and tnrs results using left join, keeping all of the database data
# 11. the remaining unresolved taxa can be manually edited and tracked
# 12. flag any changes in scientificName or family and create two new columns for these
# 13. summarize results of the above in a table, hopefully allowing for easier decision making
# 14. bring in APG sequence
# 15. bring in collection inventory
# 16. Bind Inventory with Harmonized Specimen Data


#############################################
### 1. Read in your Pteridophyte collections data, occurence set is UNM specimens filtered to 'Lycopodiophyta, Pteridophyta', around 24k specimens
database <- read_csv("~/OneDrive - University of New Mexico/UNM Herbarium HF/Research/NM_ferns/inputs/UNMFFA10312023/occurrences.csv")


#############################################
### 2. Clean occurrences using trim function, clean accents in authorship, common name issues
str_trim(database) 
database <- database%>%mutate(across("scientificNameAuthorship", str_replace, '\xe9', 'e'))
database <- database%>%mutate(across("scientificName", str_replace, '\xe9', 'e'))
database <- database%>%mutate(across("family", str_replace, 'Isodtaceae', 'Isoetaceae'))
database <- database%>%mutate(across("genus", str_replace, 'IsoÎtes', 'Isoetes'))


#############################################
### 3. Run TNRS using wcvp as the taxonomic source
## with some data subsetting for better TNRS runs

## Get a unique list of taxonomic names
db_count <- database %>% count(scientificName, genus, family) #305 unique names
db_count_taxa <- database %>% count(scientificName) #289 (disparity between two shows diff family/sci name names in database/inconsistent nomenclature)

##  Subset higher taxonomy for later on (genus and higher). We have 46 in this case. 3 to family, 43 to genus.
db_high <- database %>%
  filter(
    taxonRank == "Genus" | taxonRank == "Family" | taxonRank == "Order"
  )

## get rid of data frame for easier tnrs harmonization 
db_spp <- c(database$scientificName) #not unique, use this for taxize

## Resolve names using TNRS
db_tax <-TNRS(taxonomic_names = db_spp, sources = c("wcvp")) 


#############################################
### 4. Look over the results....
## 5 with warnings of partial match, where name matched at a higher taxon than the name submitted, this feels fine, and another handful with less than 100%
## 5 ddint run, the same 5 that never run


#############################################
### 5. Run TNRS for unresolved taxa names (no opinoin, no submit, and na) 
# Pull out names that were not resolved to an accepted name  (NA)
#####issue: what is being duplicated here? 
unresolved_tnrs <- db_tax %>%
  filter(is.na(Accepted_name))

## turn unresolved names into simpler list so it actually runs
unresolved_ls <- c(unresolved_tnrs$scientificName)

## Retrieve names not even submitted to TNRS for whatever reason
`%notin%` <- Negate(`%in%`) # negates the command %in%
taxa <- unique(database$scientificName) # a list of unique taxa names from UNM database, 290
no_submit <- taxa[taxa %notin% tnrs_result$Name_submitted] # retrieve names in the UNM database that are not found in the tnrs column "Names_submitted", 3 names

## Retrieve names returning no opinion in TNRS using "wcvp" as the source and remove these from the tnrs dataframe
no_opinion <- tnrs_result %>% filter(Taxonomic_status == "No opinion") %>% with(Name_submitted) # 5 taxa names

# These taxa names were unable to be resolved ("No opinion"):
#"Notholaena standleyi"                   
#"Adiantum capillus"                     
#"Argyrochosma limitanea subsp. mexicana" 
#"Pellaea ternifolia subsp. arizonica"   
#"Pellaea glabella subsp. simplex" 

## Run TNRS for those names that remain unresolved, including the higher taxa extracted earlier
tnrs_result_rest <- TNRS(unresolved_ls$scientificName, sources = "wcvp")
tnrs_result_high <- TNRS(db_high$scientificName, sources = "wcvp")
tnrs_another <- TNRS(c(no_opinion, no_submit), sources = "wcvp") ## 3 names not getting taxized
## Review results of these runs 

#do lines for specific taxa if needed, also try another source
## maybe try a diff source other than wcvp, wpl might be best, tropicos, usda, wfo, wcvp 
individual_taxa_1 <- TNRS("Notholaena standleyi", source = "wcvp", "wfo")
individual_taxa_2 <- TNRS("Argyrochosma limitanea subsp. mexicana", source = "tropicos")
individual_taxa_3 <- TNRS("Pellaea glabella subsp. simplex", source = "tropicos")
## if still not working, write for these names to either maintain as same and cross over or to flag for C decision or literally give up
## try and write some lines for taxa that keeps giving weird names like Hemionitis and Physmeatium


#############################################
### 6. Run TNRS for unresolved taxa names using wfo as the taxonomic source
tnrs2 <- TNRS(c(no_opinion, no_submit, unresolved_ls), sources = "wfo")


#############################################
### 7. Retrieve names not submitted to TNRS again for whatever reason
taxa2 <- unique(c(no_opinion, no_submit, unresolved_ls)) # a list of unique taxa names from UNM database
no_submit2 <- taxa2[taxa2 %notin% tnrs_another$Name_submitted] # retrieve names in the UNM database that are not found in the tnrs column "Names_submitted" 
## no names in no_submit2 means that all taxa were submitted and returned a synonym, accepted name, or no opinion


#############################################
### 8. Retrieve names returning no opinion in TNRS using "wfo" as the source and remove from the tnrs2 data frame
no_opinion2 <- tnrs_another %>% filter(Taxonomic_status == "No opinion") %>% with(Name_submitted)
tnrs2 <- tnrs2 %>% filter(Taxonomic_status != "No opinion")

# Names without an opinion according to either wcvp or wfo:
#"Notholaena standleyi"                   
#"Argyrochosma limitanea subsp. mexicana"
#"Pellaea glabella subsp. simplex" 


#############################################
### 9. Use rbind to combine tnrs_another and tnrs2 into tnrs_all. 251 of 254 taxa accounted for here. The remaining 3 taxa are in "no_opinion2"
tnrs_result <- rbind(db_tax, tnrs_result_high)
tnrs_result <- rbind(tnrs_result, tnrs_result_rest)
tnrs_result <- rbind(tnrs_result, tnrs_another) 
tnrs_result <- rbind(tnrs_result, tnrs2) %>% rename(scientificName = Name_submitted)


#############################################
### 10. Left join to add tnrs resolution data to the database table. Keep all rows in database.
## error: Detected an unexpected many-to-many relationship between `x` and `y`.
#ℹ Row 101 of `x` in database matches multiple rows in `y` tnrs all.
#ℹ Row 1 of `y` matches multiple rows in `x`.
#ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
### still works and joins seemingly
tnrs_all <- rename(tnrs_result, scientificName = Name_submitted)
database_tnrs <- left_join(database, tnrs_all, by="scientificName")


#############################################
### 11. Manual cleaning of empty and NA, along with whatever needed manual changes for disagreed harmonization
## i dont want wcvp to use Aspleniaceae for Athyrium, so manual change for Athyriaceae
## use source column to track manual changes, make a line, change source to manual
## case when 'accepted name' and 'accepted fam' are empty or NA, move family and sci name over to those columns and change service to manual
database_tnrs <- database_tnrs %>%
      mutate(
        Source = case_when(
          Accepted_name == "" ~ "manual",
          is.na(Accepted_name) ~ "manual",
          is.na(Accepted_family) ~ "manual",
          Accepted_family == "" ~ "manual",
          genus == "Athyrium" ~ "manual",
          .default = Source
        ),
        Accepted_name = case_when(
          Accepted_name == "" ~ scientificName,
          is.na(Accepted_name) ~ scientificName,
          .default = Accepted_name
        ), 
        Accepted_family = case_when(
          Accepted_family == "" ~ family,
          is.na(Accepted_family) ~ family,
          genus == "Athyrium" ~ "Athyriaceae",
          .default = Accepted_family
        ))  
        
## write to a csv to explore if you want
## write.csv(tnrs_result OR tnrs_final, "~/OneDrive - University of New Mexico/UNM Herbarium HF/Research/NM_ferns/outputs/tnrs_result_1031.csv")
        
#############################################
### 12. Check to see if the scientific or family names have changed according to tnrs. Flag if yes by adding new columns.Also adding new columns here.
database_tnrs <- database_tnrs %>% 
  mutate(
    nameChanged = case_when(scientificName != Accepted_name ~ "Yes", .default = "No"),
    familyChanged = case_when(family != Accepted_family ~ "Yes", .default = "No"),
    CMdecisionFAM = "",
    CMdecisionLOW = "",
    Accepted_genus = word(Accepted_name, 1)
  )


#############################################
### 13. Bring in Phylogenetic Sequence Numbers
sequence <- read.csv("~/OneDrive - University of New Mexico/UNM Herbarium HF/Research/NM_ferns/inputs/PPGsequence.csv")
sequence_fam <- sequence %>% select(FamilySequence, Family) %>% distinct()

# Add PPG numbers to the db_count_new dataframe
database_tnrs <- left_join(database_tnrs, sequence_fam %>% rename(Accepted_family = Family), by="Accepted_family") 
## need to add a case when statement here for no opinoin taxa that werent resolved, to use family there so number is still added


#############################################
### 14. Summarize changes and taxon counts in database_summary
database_summary <- database_tnrs %>% 
  select(
    family,
    genus,
    scientificName,
    scientificNameAuthorship,
    FamilySequence,
    Accepted_family,
    Accepted_genus,
    Accepted_name,
    Accepted_name_author,
    nameChanged,
    familyChanged,
    CMdecisionFAM,
    CMdecisionLOW,
    Source,
  ) %>%
  add_count(scientificName) %>%
  unique() 


#############################################
### 15. Clean Genus Inventory File:
## We want to create a large table that includes collections data by genus and the DwC archive data, 
## to identify any taxa that need to be moved to a new family within the collections. 
## Therefore we need to clean up the inventory file to put each genus on a single line. 
## This is where folder number is helpful - if anything is flagged as having been taxonomically revised into a new family, 
## we can look at how many folders may need to be moved. NumCubbies is not as useful, 
## because some cubbies hold 10+ genera but all genera will say "1" for NumCubbies value.

# read in inventory file
inventory <- read_csv("~/OneDrive - University of New Mexico/UNM Herbarium HF/Research/NM_ferns/inputs/2022CollectionGenusInventory.csv") %>% 
  filter(
    group == "Ferns and allies"
  )

# trim genus inventory file to just taxonomic information and space occupied
inventory_trim <- inventory %>%
  select(
    family, 
    genus, 
    numFolders
  )

# consolidate data for genera that are on multiple lines (across cabinets) 
inventory_clean <- inventory_trim %>%
  group_by(
    family, 
    genus) %>% 
  summarise(
    numFolders = sum(numFolders)
  )

#############################################
### 16. Bind Inventory with Harmonized Specimen Data
## Join inventory with db_count_new_genus_ppg, merging old inventory with new name and harmonization, full join keeps lines from both df, left join keeps one full table and adds only corresponding lines
db_inventory <- full_join(database_summary, inventory_clean, by=c("family", "genus"))
## look at full summary to clean database issues and lack of matching, such as data entry errors, missing inventory entries, maty need to entirely rerun to this point to illuminate all these issues

### Write dataframes to File
write.csv(database_tnrs, "~/OneDrive - University of New Mexico/UNM Herbarium HF/Research/NM_ferns/outputs/unmffatnrs_nov22023.csv")
write.csv(db_inventory, "~/OneDrive - University of New Mexico/UNM Herbarium HF/Research/NM_ferns/outputs/unmppgattempt_nov22023.csv")


#############################################
###ISSUES:
##1. Entries just to genus and just to family not being put through? Can we at least look at bringing them over to the sequence? Using 'genus_matched' or 'Name-matched' so it comes out in the full output?
##2. Double names coming out in output, how to remove duplicates? or flag duplicates? How do we want to handle these?
##3. Do we need to code in the same 3 names that wont taxize at all?


###Larger issues to ask weston and ben about....
##1. Limitations of folder numbers at genus level
##2. Matching at genus and family is limited, if half of a genus is in the database under one family circumscription and the other half under another, but all filed under the old name, how do you reckon that?
##3. We should only update the family circumscription unless theres an exception, how do we decide what is the exception.
















#############################################
######## Need to decide what role cleaning inventory should play and if we want this to be apart of this at all
## Count the number of cubbies for each current family
## There is probably a more efficient way to do this. But basically, figure out how many cubbies are occupied by each family as of right now. 
## Skip if user has already counted out the number of cubbies occupied by each family.

# new dataframe specifically for this question
inventory_families <- inventory %>% 
  select(
    family
    , numCubbies
  ) %>% 
  filter(
    family != "Ferns"
  )

# make values numeric
inventory_families$numCubbies <- as.numeric(inventory_families$numCubbies)

# calculate the number of cubbies and cabinets occupied by each family in current collections
inventory_families <- inventory_families %>%
  group_by(
    family
  ) %>%
  mutate(
    cubbyTotal = sum(numCubbies)
  ) %>%
  ungroup() %>%
  select(
    family
    , cubbyTotal
  ) %>%
  distinct()

## Get numbers of specimens per family originally and after taxonomic harmonization
db_count <- database %>% count(scientificNameWithoutAuthor, genus, family)

#############






