# Download and clean tick data 
# Authors: Wynne Moss, Melissa Chen, Brendan Hobart, Matt Bitters, John Foster
# Date: 2020-10-19

### Script to create tidy dataset of NEON tick abundances 
library(tidyverse) # for data wrangling and piping (dplyr probably ok)
library(lubridate) # for finding year from dates
library(stringr) # for searching within character strings 
library(here) # for working within subdirectories
library(parallel) # for using more than one core in download

if(!"neonstore" %in% installed.packages()){
  library(remotes)
  remotes::install_github("cboettig/neonstore", ref = "patch/api-updates")
}

#Sys.setenv("NEONSTORE_HOME" = "/efi_neon_challenge/neonstore")
#Sys.setenv("NEONSTORE_DB" = "/efi_neon_challenge/neonstore")
library(neonstore)

###########################################
#  LOAD TICK DATA FROM NEON OR FILE SOURCE 
###########################################

# Tick Abundance and field data data are in DP1.10093.001

# If data weren't already downloaded, download full NEON dataset
# As of 7/6/20 loadByProduct had bugs if using the CRAN version of the package
# Downloading the package NeonUtilities via Github solves the issue

target.sites <- c("BLAN", "ORNL", "SCBI", "SERC", "KONZ", "TALL", "UKFS")

neon_download(product = "DP1.10093.001", # tick data product
              end_date = "2021-12-31",   # end date for all data
              site = target.sites,       # target sites defined from 00_Target_Species_EDA.Rmd
              type = "basic")            # tick data sets do not have "expanded" data



# tck_taxonomyProcessed-basic and tck_fielddata-basic are the two datasets we want 
# first we need to import the data into a local database
neon_store(table = "tck_taxonomyProcessed-basic")
neon_store(table = "tck_fielddata-basic")

# then grab the tables from the local db to work with locally
tck_taxonomyProcessed <- neon_table("tck_taxonomyProcessed-basic")
tck_fielddata <- neon_table("tck_fielddata-basic")

###########################################
# NOTES ON GENERAL DATA ISSUES #
###########################################

# Issue 1: the latency between field (30 days) and lab (300 days) is different
# In 2019, tick counts were switched over to the lab instead of the field
# If the samples aren't processed yet, there will be an NA for all the counts (regardless of whether ticks were present)
# Normally if ticks weren't present (targetTaxaPresent == "N) we could feel comfortable assigning all the tick counts a 0 (no ticks)
# In this case, doing that would mean that there are 0s when ticks are absent and NAs when ticks are present
# Yearly trends would show a bias since there are no counts for all the sites with ticks, and 0s when there aren't ticks
# Rather, we should not use those dates until the lab data come in
# Would be good to check with NEON whether the 0s for counts are assigned at the same time the lab data come in?

# Issue 2: larva counts
# larvae were not always counted or ID'd in earlier years
# requiring larval counts to be non-NA will remove records from earlier years

# Issue 3: discrepancies between field counts and lab counts
# Often lab counts are less than field because they stop counting at a certain limit
# This is not always recorded in the same way
# Other times ticks were miscounted or lost on either end
# Probably OK to ignore most minor issues, and make best attempt at more major issues
# Drop remaining records that are confusing

###########################################
# CLEAN TICK FIELD DATA #
###########################################

#### NAs
# replace empty characters with NAs instead of ""
repl_na <- function(x) ifelse(x=="", NA, x)

tck_fielddata %>% dplyr::mutate_if(.predicate = is.character, .funs = repl_na) -> tck_fielddata

#### Quality Flags
# remove samples that had logistical issues
# keep only those with NA in samplingImpractical 
tck_fielddata %>% 
  dplyr::filter(is.na(samplingImpractical)|samplingImpractical == "OK") -> tck_fielddata_filtered

#### Remove records with no count data  
# 2019 data collection changed, counts will always be NA 
# so we want to keep those records
tck_fielddata_2019 <- tck_fielddata_filtered %>% 
  dplyr::mutate(Year = year(collectDate)) %>% 
  dplyr::filter(Year >= 2019) %>% 
  dplyr::select(-Year)

# filter only records with count data
tck_fielddata_filtered %>% 
  dplyr::filter(!is.na(adultCount), 
                !is.na(nymphCount),
                !is.na(larvaCount)) -> tck_fielddata_filtered

tck_fielddata_filtered <- dplyr::bind_rows(tck_fielddata_filtered, tck_fielddata_2019)

# note that requiring larval counts to be non na will drop some legacy data

#### Check correspondence between field and lab
# Making the assumption that the user only cares about ID'd ticks and not raw abundances.
# If so, only include field records corresponding to the dates with lab data

# Get rid of field samples that have no taxonomic info 
tck_fielddata_filtered %>% 
  dplyr::filter(sampleID %in% tck_taxonomyProcessed$sampleID | is.na(sampleID)) -> tck_fielddata_filtered 

# Get rid of the tax samples that have no field data
# many of these are legacy samples where larvae weren't counted 
tck_taxonomyProcessed %>% 
  dplyr::filter(sampleID %in% tck_fielddata_filtered$sampleID) -> tck_tax_filtered


# Double we have check same sample IDs in both datasets
all.equal(length(unique(na.omit(tck_fielddata_filtered$sampleID))),
          length(unique(tck_tax_filtered$sampleID)))

# Make sure sample IDs are unique
duplicated.sample.id <- tck_fielddata_filtered %>% 
  dplyr::group_by(sampleID) %>% 
  dplyr::summarise(n = dplyr::n()) %>% 
  dplyr::filter(n > 1) %>% 
  dplyr::pull(sampleID)

# Sample IDs are "TALL_002.20190318" and "TALL_008.20190318" each have two rows, and they are identical, 
# so we'll remove the second instance
d.index <- which(tck_fielddata_filtered$sampleID == "TALL_002.20190318")
tck_fielddata_filtered <- tck_fielddata_filtered[-d.index[2],]

d.index <- which(tck_fielddata_filtered$sampleID == "TALL_008.20190318")
tck_fielddata_filtered <- tck_fielddata_filtered[-d.index[2],]

# check that drags with ticks present have a sample ID
tck_fielddata_filtered %>% 
  dplyr::filter(targetTaxaPresent == "Y" & is.na(sampleID)) # none are missing S.ID

# Make sure samples with ticks present have counts
tck_fielddata_filtered %>% 
  dplyr::filter(targetTaxaPresent == "Y") %>%
  dplyr::filter(is.na(adultCount) & is.na(nymphCount) & is.na(larvaCount)) %>% nrow()

# are there any 0 counts where there should be > 0?
tck_fielddata_filtered %>% 
  dplyr::filter(targetTaxaPresent == "Y") %>% 
  dplyr::mutate(totalCount = adultCount + nymphCount + larvaCount) %>% 
  dplyr::filter(totalCount == 0)

### Check for other missing count data
# list of fields that shouldn't have NAs
req_cols <- c("siteID", "plotID", "collectDate", "adultCount", "nymphCount", "larvaCount")

# all should have no NAs
tck_fielddata_filtered %>% 
  dplyr::select(all_of(req_cols)) %>% 
  dplyr::summarise_all(~sum(is.na(.))) 

rm(req_cols)

####################################
# CLEAN TICK TAXONOMY DATA 
####################################
### replace "" with NA
tck_tax_filtered %>% 
  dplyr::mutate_if(.predicate = is.character, .funs = repl_na) %>%

### only retain lab records that are also in the field dataset
dplyr::filter(sampleID %in% tck_fielddata_filtered$sampleID) %>%
dplyr::filter(sampleCondition == "OK") -> tck_tax_filtered

### check for NAs in fields 
tck_tax_filtered %>% 
  dplyr::select(everything()) %>% dplyr::summarise_all(~sum(is.na(.))) 

### create a flag for potential lab ID/count issues 
tck_fielddata_filtered$IDflag <- NA

# mark sample IDs that have taxonomy issues
# these come from remarks indicating that ticks in field were mis-ID'd
# useful for correcting counts later
tck_taxonomyProcessed %>% 
  dplyr::filter(str_detect(remarks, "insect|mite|not a tick|NOT A TICK|arachnid|spider")) %>% 
  dplyr::pull(sampleID) -> tax.issues

# add a flag for these samples in the field dataset
tck_fielddata_filtered$IDflag[which(tck_fielddata_filtered$sampleID %in% tax.issues)] <- "ID WRONG"

# sample IDs where lab reached limit and stopped counting
# will help explain why lab counts < field counts
tck_taxonomyProcessed %>% 
  dplyr::filter(str_detect(remarks, "billing limit|invoice limit")) %>% 
  dplyr::pull(sampleID) -> invoice.issues

tck_fielddata_filtered$IDflag[which(tck_fielddata_filtered$sampleID %in% invoice.issues)] <- "INVOICE LIMIT"

rm(tax.issues, invoice.issues)

### require date and taxon ID
tck_tax_filtered %>% dplyr::filter(!is.na(identifiedDate)) -> tck_tax_filtered
tck_tax_filtered %>% dplyr::filter(!is.na(acceptedTaxonID))-> tck_tax_filtered

# create a lifestage column so taxa counts can be compared to field data
tck_tax_filtered %>% 
  dplyr::mutate(lifeStage = case_when(sexOrAge == "Male" | sexOrAge == "Female" | sexOrAge == "Adult" ~ "Adult",
                                                  sexOrAge == "Larva" ~ "Larva",
                                                  sexOrAge == "Nymph" ~ "Nymph")) -> tck_tax_filtered
sum(is.na(tck_tax_filtered$lifeStage))
# this assumes that any ticks that were sexed must be adults

#########################################
#  MERGE FIELD AND TAXONOMY DATA # 
#########################################

# there are a few options here:
# 1) left join tick_field to tick_tax: this will keep all the 0s but requires some wide/long manipulation
# 2) left join tick_tax to tick_field: this will only keep the records where ticks were ID'd. could add in 0s afterwards depending on preference

# We will left join tick_field to tick_tax, so that 0s are preserved.
# Merging will require us to resolve count discrepancies (this may be overkill for those interested in P/A)
# For simplicity we will also collapse M/F into all adults (also optional)

# first check for dup colnames
intersect(colnames(tck_fielddata_filtered), colnames(tck_tax_filtered))

# rename columns containing data unique to each dataset
colnames(tck_fielddata_filtered)[colnames(tck_fielddata_filtered)=="uid"] <- "uid_field"
colnames(tck_fielddata_filtered)[colnames(tck_fielddata_filtered)=="sampleCondition"] <- "sampleCondition_field"
colnames(tck_fielddata_filtered)[colnames(tck_fielddata_filtered)=="remarks"] <- "remarks_field"
colnames(tck_fielddata_filtered)[colnames(tck_fielddata_filtered)=="dataQF"] <- "dataQF_field"
colnames(tck_fielddata_filtered)[colnames(tck_fielddata_filtered)=="publicationDate"] <- "publicationDate_field"

colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="uid"] <- "uid_tax"
colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="sampleCondition"] <- "sampleCondition_tax"
colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="remarks"] <- "remarks_tax"
colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="dataQF"] <- "dataQF_tax"
colnames(tck_tax_filtered)[colnames(tck_tax_filtered)=="publicationDate"] <- "publicationDate_tax"

# in order to merge counts with field data we first combine counts from male/female in the tax data into one adult class
# if male/female were really of interest, skip this (but the table will just be much wider since all will have a M-F option)

tck_tax_filtered %>% dplyr::group_by(sampleID, acceptedTaxonID, lifeStage) %>%
  dplyr::summarise(individualCount = sum(individualCount, na.rm = TRUE)) %>%
  # now make it wide;  only one row per sample id
  tidyr::pivot_wider(id_cols = sampleID,  names_from = c(acceptedTaxonID, lifeStage), 
                     values_from = individualCount)  -> tck_tax_wide

# replace NAs with 0s
tck_tax_wide %>% replace(is.na(.), 0) -> tck_tax_wide
# each row is now a unique sample id (e.g. a site x species matrix)
tck_fielddata_filtered %>% 
  dplyr::left_join(tck_tax_wide, by = "sampleID") -> tck_merged

#########################################################
#FIX COUNT DISCREPANCIES BETWEEN FIELD AND LAB #
#########################################################

### clean up 0s
# if ticks were not found in the field, counts should be 0
tck_merged %>% 
  dplyr::select(dplyr::contains(c("_Nymph", "_Adult" , "_Larva"))) %>% 
  colnames() -> taxon.cols # columns containing counts per taxon

for(i in 1:length(taxon.cols)){
  tck_merged[which(tck_merged$targetTaxaPresent =="N"), which(colnames(tck_merged) == taxon.cols[i])] = 0
}

# If there are NAs for counts and an ID flag, 
# most likely the lab did not find ticks and field data should be corrected
tck_merged %>% 
  dplyr::select(uid_field, taxon.cols, IDflag) %>% 
  dplyr::filter_at(vars(all_of(taxon.cols)), all_vars(is.na(.))) %>% 
  dplyr::filter(!is.na(IDflag)) %>% 
  dplyr::pull(uid_field) -> uid_change

# correct the field data to reflect no ticks
tck_merged[which(tck_merged$uid_field==uid_change), "targetTaxaPresent"] = "N"
tck_merged[which(tck_merged$uid_field==uid_change), "adultCount"] = 0
tck_merged[which(tck_merged$uid_field==uid_change), "nymphCount"] = 0
tck_merged[which(tck_merged$uid_field==uid_change), "larvaCount"] = 0
for(i in 1:length(taxon.cols)){
  tck_merged[which(tck_merged$uid_field==uid_change), which(colnames(tck_merged) == taxon.cols[i])] = 0
}

#### FIX COUNT 1.1 FLAG DISCREPANCIES #####

# first get list of columns for easy summing
tck_merged %>% select(contains("_Adult")) %>% colnames() -> adult_cols
tck_merged %>% select(contains("_Nymph")) %>% colnames() -> nymph_cols
tck_merged %>% select(contains("_Larva")) %>% colnames() -> larva_cols

# sum up lab and field totals
tck_merged %>% mutate(
  totalAdult_tax = rowSums(.[adult_cols]),
  totalNymph_tax = rowSums(.[nymph_cols]),
  totalLarva_tax = rowSums(.[larva_cols]),
  totalCount_tax = rowSums(.[c(adult_cols, nymph_cols, larva_cols)]),
  totalCount_field = rowSums(.[c("adultCount", "nymphCount", "larvaCount")])
) -> tck_merged

# check if there are any NAs in totalCount_field
tck_merged %>%
  select(totalCount_field) %>%
  filter(is.na(.)) %>%
  nrow() # yes

# check if these NAs are all 2019 or later
tck_merged %>%
  filter(collectDate >= ymd("2019-01-01")) %>%
  select(totalCount_field) %>%
  filter(is.na(.)) %>%
  nrow() # yes, same as above

# change these NAs to 0, and we will deal with this flag below
tck_merged$totalCount_field[is.na(tck_merged$totalCount_field)] <- 0

# get a list of columns with counts for easily viewing dataset (optional)
tck_merged %>% select(contains(c("adult", "nymph","larva", "total", "Adult",  "Nymph",  "Larva", "Count"), 
                               ignore.case = FALSE)) %>% select(-totalSampledArea) %>% colnames() -> countCols

# create flag column to mark issues with count discrepancies
tck_merged %>% mutate(CountFlag = NA) -> tck_merged

# flag 1: the total count matches, but the life stage columns don't (error 1)
tck_merged$CountFlag[tck_merged$totalCount_field==tck_merged$totalCount_tax] <- 1

# no flag:  all the counts match (no flag) -- some of the 1s will now be  0s
tck_merged$CountFlag[tck_merged$nymphCount==tck_merged$totalNymph_tax &
                       tck_merged$adultCount == tck_merged$totalAdult_tax &
                       tck_merged$larvaCount == tck_merged$totalLarva_tax] <- 0

# no flag: no ticks
tck_merged$CountFlag[tck_merged$targetTaxaPresent=="N"] <- 0

# flag 2: the field total is > than tax total
tck_merged$CountFlag[tck_merged$totalCount_field > tck_merged$totalCount_tax] <- 2

# flag 3: the field total is < than the tax total
tck_merged$CountFlag[tck_merged$totalCount_field < tck_merged$totalCount_tax] <- 3 

### reconcile count discrepancies
# table(tck_merged$CountFlag)
# the vast majority of counts match. 
# we expect to see error 3, because starting in 2019
# no field counts are reported, just Y/N

#### FIX COUNT 2.1 RECONCILE DISCREPANCIES WHERE TOTALS MATCH #####

### Flag Type 1: the total count from field and lab matches but not individual lifestages
tck_merged %>% filter(CountFlag == 1) %>% select(countCols) 
# many of these are adults misidentified as nymphs or vice versa
# trust the lab counts here (ignore nymphCount, adultCount, larvaCount from field data)
# don't correct any counts and change the flag from a 1 to 0 
tck_merged %>%  mutate(CountFlag = ifelse(CountFlag == 1, 0, CountFlag)) -> tck_merged
# table(tck_merged$CountFlag)


#### FIX COUNT 2.2 RECONCILE DISCREPANCIES WHERE FIELD COUNT > TAX COUNT ####
### Flag Type 2:more ticks found in the field than in the lab

# in some cases, there were too many larvae and not all were ID'd
# in other cases, there may be issues with the counts and some ended up not being ticks
# if we can't rectify, lump the excess field ticks into IXOSP2
# IXOSP2 is the least informative ID, essentially the same as marking them as "unidentified tick"

### some of these have notes in the ID flag column indicating a non-tick
# trust the lab count and don't correct the counts
# remove flag (2->0)
tck_merged %>% mutate(CountFlag = case_when(CountFlag == 2 & IDflag == "ID WRONG" ~ 0,
                                            TRUE ~ CountFlag)) -> tck_merged

### some have more larvae in the field than in the lab
# larvae weren't always identified/counted in the lab, or if they were, only counted up to a limit
# if there were more larvae in the field than in lab, the extra larvae should be added to the order level (IXOSP2)
# "extra larvae" = larvae not counted - those moved to another lifestage
tck_merged %>% mutate(IXOSP2_Larva = case_when(
  CountFlag == 2 & larvaCount>totalLarva_tax & is.na(IDflag) ~ 
    IXOSP2_Larva + (larvaCount - totalLarva_tax) + (nymphCount - totalNymph_tax) + (larvaCount - totalLarva_tax), 
  TRUE ~ IXOSP2_Larva)) -> tck_merged

# remove flag
tck_merged %>% mutate(CountFlag = case_when(
  CountFlag == 2 & larvaCount>totalLarva_tax & is.na(IDflag)~0, 
  TRUE ~ CountFlag)) -> tck_merged


### some counts are off by only a few individuals (negligible)
# could be miscounted in the field or lost in transit
# if counts are off by < 10% this difference is not too meaningful
# trust the lab count data, don't change any counts
# remove flag
tck_merged %>% mutate(CountFlag = case_when(
  CountFlag == 2 & (totalCount_field - totalCount_tax)/totalCount_field < 0.1 ~0, # if discrepancy < 10% of total
  TRUE ~ CountFlag)) -> tck_merged

### some counts are off by more than 10% but explanation is obvious
# cases where 1-2 field ticks are "missing" but discrepancy is more than 10% of total count
# if there are field remarks, assume they are about ticks being lost (see field_remarks output)
# assign missing field ticks to order level IXOSP2
# missing (unidentified) ticks = missing counts that were not assigned to other lifestages

# if IXOSP2 doesn't exist for other lifestages, add it 
if(sum(colnames(tck_merged) == "IXOSP2_Adult")==0){ # if column doesn't exist,
  tck_merged <- tck_merged %>% mutate(IXOSP2_Adult=0) # add it with a default of 0
}
if(sum(colnames(tck_merged) == "IXOSP2_Nymph")==0){
  tck_merged <- tck_merged %>% mutate(IXOSP2_Nymph=0)
}
if(sum(colnames(tck_merged) == "IXOSP2_Larva")==0){
  tck_merged <- tck_merged %>% mutate(IXOSP2_Larva=0)
}

# if there are missing adults and remarks in the field, add the missing adults to IXOSP2_Adult
tck_merged %>% mutate(IXOSP2_Adult = case_when(
  CountFlag == 2 & adultCount>totalAdult_tax & (adultCount-totalAdult_tax) <= 2 & !is.na(remarks_field) ~  
    IXOSP2_Adult+ (adultCount - totalAdult_tax) + (nymphCount-totalNymph_tax) + 
    (larvaCount-totalLarva_tax), # count discrepancy add to order IXOSP2
  TRUE ~ IXOSP2_Adult)) %>%
  # if there are missing nymphs and remarks in the field, add the missing adults to IXOSP2_Nymph
  mutate(IXOSP2_Nymph = case_when(
    CountFlag == 2 & nymphCount>totalNymph_tax & (nymphCount-totalNymph_tax) <= 2 & !is.na(remarks_field) ~
      IXOSP2_Nymph+ (adultCount - totalAdult_tax) + (nymphCount-totalNymph_tax) + 
      (larvaCount-totalLarva_tax), # count discrepancy add to order IXOSP2
    TRUE ~ IXOSP2_Nymph)) %>%
  # if there are missing larvae and remarks in the field, add the missing adults to IXOSP2_Larva
  mutate(IXOSP2_Larva = case_when(
    CountFlag == 2 & larvaCount>totalLarva_tax & (larvaCount-totalLarva_tax) <= 2 & !is.na(remarks_field) ~
      IXOSP2_Larva+ (adultCount - totalAdult_tax) + (nymphCount-totalNymph_tax) + (larvaCount-totalLarva_tax), 
    TRUE ~ IXOSP2_Larva)) -> tck_merged


# remove flag
tck_merged %>% mutate(CountFlag = case_when(
  CountFlag == 2 & adultCount>totalAdult_tax & (adultCount-totalAdult_tax) <= 2 & !is.na(remarks_field)~0,
  CountFlag == 2 & nymphCount>totalNymph_tax & (nymphCount-totalNymph_tax) <=2 & !is.na(remarks_field)~0, 
  CountFlag == 2 & larvaCount>totalLarva_tax & (larvaCount-totalLarva_tax) <=2 & !is.na(remarks_field)~0, 
  TRUE~ CountFlag)) -> tck_merged


### some counts are off because over invoice limit 
# in this case trust the field counts, and extra are assigned to order level
tck_merged %>% mutate(IXOSP2_Nymph = case_when(
  CountFlag == 2 & nymphCount>totalNymph_tax & IDflag == "INVOICE LIMIT" ~
    IXOSP2_Nymph+ (adultCount - totalAdult_tax) + (nymphCount-totalNymph_tax) + 
    (larvaCount-totalLarva_tax), # add count discrepancy to order
  TRUE ~ IXOSP2_Nymph)) %>%
  mutate(IXOSP2_Larva = case_when(
    CountFlag == 2 & larvaCount>totalLarva_tax & IDflag == "INVOICE LIMIT" ~
      IXOSP2_Larva+ (adultCount - totalAdult_tax) + (nymphCount-totalNymph_tax) + 
      (larvaCount-totalLarva_tax), # add count discrepancy to order
    TRUE ~ IXOSP2_Larva)) %>%
  mutate(IXOSP2_Adult = case_when(
    CountFlag == 2 & adultCount>totalAdult_tax & IDflag == "INVOICE LIMIT" ~
      IXOSP2_Adult+ (adultCount - totalAdult_tax) + (nymphCount-totalNymph_tax) + 
      (larvaCount-totalLarva_tax), # add count discrepancy to order
    TRUE ~ IXOSP2_Adult))-> tck_merged

# remove flag
tck_merged %>% mutate(CountFlag = case_when(
  CountFlag == 2 & adultCount>totalAdult_tax &IDflag == "INVOICE LIMIT" ~ 0,
  CountFlag == 2 & nymphCount>totalNymph_tax &IDflag == "INVOICE LIMIT" ~ 0, 
  CountFlag == 2 & larvaCount>totalLarva_tax &IDflag == "INVOICE LIMIT" ~ 0, 
  TRUE ~ CountFlag)) -> tck_merged

# table(tck_merged$CountFlag) 

#### FIX COUNT 2.3 RECONCILE DISCREPANCIES WHERE FIELD COUNT < TAX COUNT
# remove flag for 2019 records or later
tck_merged %>% mutate(CountFlag = case_when(
  CountFlag == 3 & collectDate >= ymd("2019-01-01") ~ 0
)) -> tck_merged

### cases where counts are off by minor numbers 
# this could be miscounts in the field
# if the discrepancy is less than 30% of the total count or 5 or less individuals
# trust the lab count here and don't correct counts

# remove flag
tck_merged %>% mutate(CountFlag = case_when(
  CountFlag == 3 & (abs(totalCount_tax-totalCount_field))/totalCount_tax < 0.3 ~ 0,
  (totalCount_tax - totalCount_field)<=5 ~ 0,
  TRUE ~ CountFlag)) -> tck_merged

#### FIX COUNT 2.4 REMOVE THE UNSOLVED MYSTERY SAMPLES
# cases with larger discrepancies that aren't obvious are removed from final dataset
# make sure this is less than 1% of total cases
# more work could be done here if there are many cases in this category
if((tck_merged %>% filter(CountFlag!=0) %>% nrow())/nrow(tck_merged)< 0.01){
  tck_merged <- tck_merged %>% filter(CountFlag==0)
}

#### Final notes on count data
# note that some of these could have a "most likely" ID assigned based on what the majority of IDs are
# for now don't attempt to assign. 
# the user can decide what to do with the IXOSP2 later (e.g. assign to lower taxonomy)
# from this point on, trust lab rather than field counts because lab were corrected if discrepancy exists

#####################################################
# RESHAPE FINAL DATASET TO tck_merged_final
######################################################

# will depend on the end user but creating two options
# note that for calculation of things like richness, might want to be aware of the level of taxonomic resolution

tck_merged %>% 
  select(contains(c("_Larva", "_Nymph", "_Adult"))) %>% 
  colnames() -> taxon.cols # columns containing counts per taxon


### option 1) long form data including 0s

# get rid of excess columns (user discretion)
tck_merged_final <- tck_merged %>% select(-CountFlag, -IDflag, -totalAdult_tax, 
                                          -totalNymph_tax, -totalLarva_tax, -totalCount_tax, 
                                          -totalCount_field,
                                          -nymphCount, -larvaCount, -adultCount, -coordinateUncertainty, 
                                          -elevationUncertainty,
                                          -samplingImpractical, -measuredBy)


# long form data
tck_merged_final %>% pivot_longer(cols = all_of(taxon.cols), 
                                  names_to = "Species_LifeStage", values_to = "IndividualCount") %>%
  separate(Species_LifeStage, into = c("acceptedTaxonID", "LifeStage"), 
           sep = "_", remove = FALSE) -> tck_merged_final


# add in taxonomic resolution
table(tck_taxonomyProcessed$acceptedTaxonID, tck_taxonomyProcessed$taxonRank)# accepted TaxonID is either at family, order, or species level
table(tck_taxonomyProcessed$acceptedTaxonID, tck_taxonomyProcessed$scientificName)
tck_taxonomyProcessed %>% group_by(acceptedTaxonID) %>% 
  summarise(taxonRank = first(taxonRank), scientificName= first(scientificName)) -> tax_rank

tck_merged_final %>% left_join(tax_rank, by = "acceptedTaxonID") -> tck_merged_final

# only most abundant taxa (amblyomma and ixodes)

amb.plots <- c("SCBI_013","SERC_001","SERC_005","SERC_006","SERC_002","SERC_012","KONZ_025",
               "UKFS_001","UKFS_004","UKFS_003","ORNL_002","ORNL_040","ORNL_008","ORNL_007",
               "ORNL_009","ORNL_003","TALL_001","TALL_008","TALL_002")
ix.plots <- c("BLAN_012","BLAN_005","SCBI_013","SCBI_002","SERC_001","SERC_005","SERC_006",
              "SERC_012","ORNL_007")

# filter to just target data
# FILTER TO EACH SPECIES FIRST THEN COMBINE
ambame.target.data <- tck_merged_final %>% 
  filter(LifeStage == "Nymph",
         acceptedTaxonID == "AMBAME",
         plotID %in% amb.plots) %>% # just the species 
  mutate(Year = year(collectDate),    # add year column
         epiWeek = sprintf("%02d", epiweek(collectDate)), # add week column, leading 0 for single digits
         yearWeek = as.numeric(paste0(Year, epiWeek)), # yearWeek column
         targetSpecies = "Amblyomma_americanum",
         targetCount = IndividualCount,
         plotID = plotID) %>%   
  select(all_of(c("Year", "epiWeek", "yearWeek", "targetSpecies", "targetCount", 
                  "plotID", "siteID", "nlcdClass", "decimalLatitude", 
                  "decimalLongitude", "elevation", "totalSampledArea", "eventID")))
ix.target.data <- tck_merged_final %>% 
  filter(LifeStage == "Nymph",
         acceptedTaxonID == "IXOSCA",
         plotID %in% ix.plots) %>% # just the species
  mutate(Year = year(collectDate),    # add year column
         epiWeek = sprintf("%02d", epiweek(collectDate)), # add week column, leading 0 for single digits
         yearWeek = as.numeric(paste0(Year, epiWeek)), # yearWeek column
         targetSpecies = "Ixodes_scapularis",
         targetCount = IndividualCount,
         plotID = plotID) %>%   
  select(all_of(c("Year", "epiWeek", "yearWeek", "targetSpecies", "targetCount", 
                  "plotID", "siteID", "nlcdClass", "decimalLatitude", 
                  "decimalLongitude", "elevation", "totalSampledArea", "eventID")))

# both species in one tibble
tick.target.data <- bind_rows(ambame.target.data, ix.target.data)

tick.target.data <- tick.target.data %>% 
  group_by(Year, epiWeek, plotID) %>%
  mutate(row = row_number(),
         epiWeek = as.integer(epiWeek)) %>%
  pivot_wider(names_from = targetSpecies, values_from = targetCount) %>% 
  select(-row) %>% 
  mutate(time = MMWRweek::MMWRweek2Date(Year, epiWeek))

# need to collapse the plotIDs where both species are observed
# but need to retain all other NAs for weeks without observations
both.spp.filter <- tick.target.data %>%
  group_by(Year, epiWeek, yearWeek, plotID, siteID, nlcdClass, time,
           decimalLatitude, decimalLongitude, elevation, totalSampledArea) %>%
  filter(plotID %in% ix.plots[which(ix.plots %in% amb.plots)])

# separate species
ix.from.both <- both.spp.filter %>% 
  select(-Amblyomma_americanum)
aa.from.both <- both.spp.filter %>% 
  select(-Ixodes_scapularis)

# join species and keep the good row
both.spp.join <- left_join(ix.from.both, aa.from.both) %>% 
  filter(!is.na(Ixodes_scapularis), !is.na(Amblyomma_americanum))

# put everything back together 
tick.target.data <- tick.target.data %>% 
  filter(!(plotID %in% ix.plots[which(ix.plots %in% amb.plots)])) %>% 
  bind_rows(., both.spp.join) %>% 
  mutate(epiWeek = as.character(epiWeek)) %>% 
  distinct(eventID, .keep_all = TRUE)

# next we need to fill in the data set for weeks that are not represented
# in tick.target.data - only has weeks that are sampled, we want to add
# all weeks from the first sampling event to the last sampling event of each year

tick.plots <- tick.target.data %>% 
  pull(plotID) %>% 
  unique()

# find the start and end week for each plot each year
start.week <- tick.target.data %>% 
  group_by(plotID, Year) %>% 
  summarise(startWeek = min(yearWeek))
end.week <- tick.target.data %>% 
  group_by(plotID, Year) %>% 
  summarise(endWeek = max(yearWeek))

for(i in seq_along(tick.plots)){
  plot.subset <- tick.plots[i]
  
  # first week of sampling each year
  first.weeks <- start.week %>%
    filter(plotID == plot.subset)
  
  # last week of sampling each year
  last.weeks <- end.week %>%
    filter(plotID == plot.subset)
  
  # all years sampling occurred in the plot
  year.vec <- pull(last.weeks, Year)
  
  # plot has the same characteristics across rows
  # pulling out a row for the subsetted plot
  # using these to fill in data set below
  row.fill <- tick.target.data %>% 
    ungroup() %>% 
    filter(plotID == plot.subset) %>% 
    select(all_of(c("time", "siteID", "nlcdClass", "decimalLatitude", "decimalLongitude", "elevation"))) %>% 
    slice(1)
  
  # weeks that have observations, for removing duplicates below
  existing.yearWeeks <- tick.target.data %>% 
    filter(plotID == plot.subset) %>%
    pull(yearWeek) %>% 
    unique() %>% 
    as.numeric()
  
  # go through each year in the subsetted plot
  for (y in year.vec) {
    
    # beginning week in year y
    begin <- first.weeks %>%
      filter(Year == y) %>%
      pull(startWeek) 
    
    # end week in year y
    end <- last.weeks %>%
      filter(Year == y) %>%
      pull(endWeek)
    
    # week sequence +/- 1, don't want to duplicate weeks
    # also need to remove weeks that have observations
    week.seq <- seq(begin + 1, end - 1) 
    week.seq <- week.seq[which(!week.seq %in% existing.yearWeeks)] 
    
    # add filler rows, columns not specified get NA
    tick.target.data <- tick.target.data %>% 
      ungroup() %>% 
      add_row(yearWeek = week.seq, 
              plotID = plot.subset,
              Year = y,
              epiWeek = str_extract(week.seq, "\\d{2}$"), # last two digits in week.seq = epiWeek number
              siteID = pull(row.fill, siteID),
              nlcdClass = pull(row.fill, nlcdClass),
              decimalLatitude = pull(row.fill, decimalLatitude),
              decimalLongitude = pull(row.fill, decimalLongitude),
              elevation = pull(row.fill, elevation),
              time = pull(row.fill, time))
    
  }
  
}

###########################################
# ENVIORNMENTAL VARIABLES
###########################################

neon_download(product = "DP4.00001.001", # Summary weather statistics
              end_date = "2021-12-31",   # end date for training data
              site = target.sites,       # target sites defined from 00_Target_Species_EDA.Rmd
              type = "basic")    

# quality flags (QF): pass = 0, fail = 1
daily.temp <- neon_read(table = "wss_daily_temp-basic")

# filter to make sure to use records that pass
# and create year/week columns as above
daily.temp.filtered <- daily.temp %>% 
  filter(tempTripleQF == 0) %>% 
  select(all_of(c("date", "siteID", "wssTempTripleMinimum", "wssTempTripleMaximum", 
                  "wssTempTripleVariance"))) %>% 
  mutate(Year = year(date),    # add year column
         epiWeek = sprintf("%02d", epiweek(date)), # add week column, leading 0 for single digits
         yearWeek = as.character(paste0(Year, epiWeek))) 

# weekly recorded minimum temperature
weekly.min.temp <- daily.temp.filtered %>% 
  group_by(siteID, yearWeek) %>%
  slice(which.min(wssTempTripleMinimum)) %>% # use slice to retain variance column
  select(-c(date, wssTempTripleMaximum)) %>% # drop columns we don't need
  rename(airTempMin_degC = wssTempTripleMinimum,
         airTempMin_variance = wssTempTripleVariance) 

# weekly recorded maximum temperature
weekly.max.temp <- daily.temp.filtered %>% 
  group_by(siteID, yearWeek) %>%
  slice(which.max(wssTempTripleMaximum)) %>% 
  select(-c(date, wssTempTripleMinimum)) %>% 
  rename(airTempMax_degC = wssTempTripleMaximum,
         airTempMax_variance = wssTempTripleVariance) 

# join data by common columns
weekly.temp.data <- left_join(weekly.min.temp, 
                              weekly.max.temp, 
                              by = c("siteID", "Year", "epiWeek", "yearWeek"))



# relative humidty, same procedure as temperature 
daily.rh <- neon_read(table = "wss_daily_humid-basic")
daily.rh.filtered <- daily.rh %>% 
  filter(RHQF == 0) %>% 
  select(all_of(c("date", "siteID", "wssRHMinimum", "wssRHMaximum", 
                  "wssRHVariance")))  %>% 
  mutate(Year = year(date),    # add year column
         epiWeek = sprintf("%02d", epiweek(date)), # add week column, leading 0 for single digits
         yearWeek = as.character(paste0(Year, epiWeek)))

# weekly recorded minimum relative humidity
weekly.min.rh <- daily.rh.filtered %>% 
  group_by(siteID, yearWeek) %>%
  slice(which.min(wssRHMinimum)) %>% # use slice to retain variance column
  select(-c(date, wssRHMaximum)) %>% # drop columns we don't need
  rename(RHMin_precent = wssRHMinimum,
         RHMin_variance = wssRHVariance) 

# weekly recorded maximum relative humidity
weekly.max.rh <- daily.rh.filtered %>% 
  group_by(siteID, yearWeek) %>%
  slice(which.max(wssRHMaximum)) %>% 
  select(-c(date, wssRHMinimum)) %>% 
  rename(RHMax_precent = wssRHMaximum,
         RHMax_variance = wssRHVariance) 

# join data by common columns
weekly.rh.data <- left_join(weekly.min.rh, 
                            weekly.max.rh, 
                            by = c("siteID", "Year", "epiWeek", "yearWeek"))


# join temp and rh together
weekly.envionmental.data <- left_join(weekly.rh.data,
                                      weekly.temp.data,
                                      by = c("siteID", "Year", "epiWeek", "yearWeek"))


# make sure epiWeek col is character
tick.target.data <- tick.target.data %>% 
  mutate(epiWeek = as.character(epiWeek),
         yearWeek = as.character(yearWeek))

# join environmental to tick data
target.data.final <- left_join(tick.target.data,
                               weekly.envionmental.data,
                               by = c("siteID", "Year", "epiWeek", "yearWeek"))

# Download the latest daily Southern Oscillation Index data file from BOM Longpaddock and add to the data
soi <- read.table('https://data.longpaddock.qld.gov.au/SeasonalClimateOutlook/SouthernOscillationIndex/SOIDataFiles/DailySOI1887-1989Base.txt',
                  skip = 1, col.names = c('year', 'day','tahiti','darwin', 'soi')) %>%
  dplyr::mutate(date = as.Date(day, origin = paste0(year, "-01-01"))) %>%
  dplyr::mutate(epiWeek = as.character(MMWRweek::MMWRweek(date)[,2])) %>%
  dplyr::group_by(year, epiWeek) %>%
  dplyr::summarise(soi = mean(soi, na.rm = T)) %>%
  dplyr::rename(Year = year)
target.data.final %>%
  dplyr::left_join(soi) -> target.data.final

# Download Daniel Ortiz's supplied NOAA NCDC GHCN_Daily Global Historical Climatology Network data for each site
links <- c('https://drive.google.com/uc?id=1BPO5p89SS3l5O584u89Sj-Maj3mmdSPF&export=download',
           'https://drive.google.com/uc?id=12gaoToniFtrwjaBxIm5MAjWO4R3QU_en&export=download',
           'https://drive.google.com/uc?id=1jXoPKbLtwph4QPp2d13tjPiXip5Sn8JY&export=download',
           'https://drive.google.com/uc?id=1sitAghr-oZ5sqxhd9OeSZ0IxqLUKuH2s&export=download',
           'https://drive.google.com/uc?id=1UkyOZEvilWdcOXrtl1dfIktfzjRWWrb_&export=download',
           'https://drive.google.com/uc?id=1CieFUiqkaUXCPu-Z7h1uoFf7g3zSDYL3&export=download',
           'https://drive.google.com/uc?id=1AnJPP7a35f6GSL3ekeSmjgu9FwULlhOs&export=download')
siteIDs <- c('BLAN','SCBI', 'SERC', 'ORNL', 'KONZ', 'UKFS', 'TALL')

# For each site, calculate annual cumulative growing degree days (above 0 celsius) up to the start of the tick season
# (begninning of June day 153)
# Also calculate cumulative suppression degree days (dry and with mean temp above 23 celsius) up to peak period of
# tick season (July - day 182)
target.data.final %>%
  dplyr::left_join(do.call(rbind, lapply(seq_along(siteIDs), function(x){
    # Download the data from the Google Drive link
    read.csv(links[x]) %>%
    dplyr::mutate(date = lubridate::date(GHCNDDATE)) %>%
    dplyr::mutate(Year = lubridate::year(date),
                  day = lubridate::yday(date)) %>%
      dplyr::filter(day != 366) %>%
    dplyr::mutate(epiWeek = as.character(MMWRweek::MMWRweek(date)[,2])) %>%
    dplyr::filter(Year > 2000) -> test
    
    test %>%
    # Add missing day / year combinations
    dplyr::right_join(expand.grid(Year = seq(min(test$Year), max(test$Year)),
                day = seq(1, 365))) %>%
    dplyr::arrange(Year, day)-> test
    
    # Censor outrageous temp values (occasional -40s for example) and impute NAs using seasonal random walks
    tmin_ts <- ts(test$TMIN, frequency = 365, start = c(test$Year[1], test$day[1]))
    tmin_ts[tmin_ts > quantile(tmin_ts, 0.99, na.rm = T)] <- quantile(tmin_ts, 0.99, na.rm = T)
    tmin_ts[tmin_ts < quantile(tmin_ts, 0.01, na.rm = T)] <- quantile(tmin_ts, 0.01, na.rm = T)
    tmin_ts[which(is.na(tmin_ts))] <- as.vector(forecast::na.interp(forecast::snaive(tmin_ts)$fitted))[which(is.na(tmin_ts))]
    test$TMIN <- as.vector(tmin_ts)

    tmax_ts <- ts(test$TMAX, frequency = 365, start = c(test$Year[1], test$day[1]))
    tmax_ts[tmax_ts > quantile(tmax_ts, 0.99, na.rm = T)] <- quantile(tmax_ts, 0.99, na.rm = T)
    tmax_ts[tmax_ts < quantile(tmax_ts, 0.01, na.rm = T)] <- quantile(tmax_ts, 0.01, na.rm = T)
    tmax_ts[which(is.na(tmax_ts))] <- as.vector(forecast::na.interp(forecast::snaive(tmax_ts)$fitted))[which(is.na(tmax_ts))]
    test$TMAX <- as.vector(tmax_ts)

    # Repeat for precipitation (no need to censor as measurements seem to all be reasonable for precip)
    prcp_ts <- ts(test$PRCP, frequency = 365, start = c(test$Year[1], test$day[1]))
    prcp_ts[which(is.na(prcp_ts))] <- as.vector(forecast::na.interp(forecast::snaive(prcp_ts)$fitted))[which(is.na(prcp_ts))]
    test$PRCP <- as.vector(prcp_ts)
    
    # Calculate cumulative growing and suppression degree days per year and return
    test %>%
    dplyr::mutate(include_in_gdd = ifelse(day < 153, 'yes', 'no')) %>%
    dplyr::mutate(gdd = ifelse(((TMAX + TMIN) / 2) > 0, (TMAX + TMIN) / 2, 0)) %>%
    dplyr::group_by(Year, include_in_gdd) %>%
    dplyr::mutate(cum_gdd = sum(gdd, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(include_in_gdd == 'yes') %>%
    dplyr::mutate(siteID = siteIDs[x]) %>%
    dplyr::select(siteID, Year, cum_gdd) %>%
    dplyr::distinct() -> test_gdd
    
    test %>%
      dplyr::mutate(include_in_sdd = ifelse(day < 182, 'yes', 'no')) %>%
      dplyr::mutate(sdd = ifelse(((TMAX + TMIN) / 2) > 23 & PRCP < 0.5, (TMAX + TMIN) / 2, 0)) %>%
      dplyr::group_by(Year, include_in_sdd) %>%
      dplyr::mutate(cum_sdd = sum(sdd, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(include_in_sdd == 'yes') %>%
      dplyr::mutate(siteID = siteIDs[x]) %>%
      dplyr::select(siteID, Year, cum_sdd) %>%
      dplyr::distinct() %>%
      dplyr::left_join(test_gdd)
    
    
  }))) -> target.data.final

# arrange for csv
target.data.final <- target.data.final %>% 
  group_by(plotID) %>% 
  arrange(yearWeek, .by_group = TRUE) %>% 
  rename(amblyomma_americanum = Amblyomma_americanum,
         ixodes_scapularis = Ixodes_scapularis) %>% 
  mutate(time = MMWRweek::MMWRweek2Date(Year, as.numeric(epiWeek))) %>% 
  select(-eventID)

# write targets
save(target.data.final, file = 'Processed_data/all_neon_tick_data.rda')
