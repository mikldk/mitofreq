## code to prepare gnomAD datasets

library(tidyverse)
library(readxl)
library(here)
library(stringi)
library(jsonlite)

################################################################################
# Column must be named HG
rename_TLHGs <- function(d) {
  d |> 
    mutate(HG = case_when(
      HG == "X + S" ~ "X",
      HG == "L4 + L5 + L6" ~ "L4-6",
      HG == "L4" ~ "L4-6",
      HG == "L5" ~ "L4-6",
      HG == "L6" ~ "L4-6",
      HG == "R" | HG == "B" ~ "R/B",
      TRUE ~ HG
    )) 
}
################################################################################

gnomad_file <- tempfile()
download.file(url = "https://storage.googleapis.com/gcp-public-data--gnomad/release/3.1/vcf/genomes/gnomad.genomes.v3.1.sites.chrM.vcf.bgz", 
              destfile = gnomad_file)


helix_suppmat_file <- tempfile()
download.file(url = "https://www.biorxiv.org/content/biorxiv/early/2020/06/26/798264/DC1/embed/media-1.xlsx?download=true", 
              destfile = helix_suppmat_file)
# Table_S1

######
d_HG_n_L <- readxl::read_excel(helix_suppmat_file, 
                               sheet = "Table_S1", 
                               range = "A3:B9", 
                               col_names = c("HG", "n"))
d_HG_n_M <- readxl::read_excel(helix_suppmat_file, 
                               sheet = "Table_S1", 
                               range = "A12:B18", 
                               col_names = c("HG", "n"))
d_HG_n_N <- readxl::read_excel(helix_suppmat_file, 
                               sheet = "Table_S1", 
                               range = "A21:B39", 
                               col_names = c("HG", "n"))
d_TLHG_n <- bind_rows(d_HG_n_L, d_HG_n_M, d_HG_n_N) |> 
  rename(N = n) |> 
  mutate(N = as.integer(N)) |> 
  rename_TLHGs() |> 
  group_by(HG) |> 
  summarise(N = sum(N), .groups = "drop") |> 
  rename(TLHG = HG)



################################################################################
d_helix_raw <- read.table(helix_db_file, sep = "\t", header = TRUE) |> 
  as_tibble()

# No strings are quoted, we need that for the jsonlite parsing to work
quote_strings <- function(x) {
  # Originally use: 
  # x <- gsub("([^A-Z]*)([A-Z]+)([^A-Z]*)", "\\1\"\\2\"\\3", x)
  # But to account for L0-L6 use this:
  x <- gsub("([^A-Z]*)([A-Z]+[0-6]*)([^A-Z]*)", "\\1\"\\2\"\\3", x)
  x
}
quote_strings("[L5,1]")
d_helix_raw_quoted <- d_helix_raw |> 
  mutate(alleles = quote_strings(alleles)) |> 
  mutate(haplogroups_for_homoplasmic_variants = quote_strings(haplogroups_for_homoplasmic_variants)) |> 
  select(locus, alleles, counts_hom, haplogroups_for_homoplasmic_variants)
d_helix_raw_quoted


d_tmp_d_helix <- d_helix_raw_quoted |> 
  mutate(Position = gsub("chrM:", "", locus, fixed = TRUE) |> as.integer()) |> 
  rowwise() |> 
  mutate(A = list(jsonlite::parse_json(alleles))) |> 
  mutate(LongestAllele = A[[which.max(sapply(A, nchar))]]) |> 
  ungroup() |> 
  mutate(LongestAlleleLength = nchar(LongestAllele))

####################################################################

exclude_positions <- function(d, d_pos, reason) {
  if (!("ExcludeReason" %in% colnames(d))) {
    d <- d |> 
      mutate(ExcludeReason = NA_character_)
  }
  
  d |> 
    left_join(d_pos |> mutate(ExcludeReasonTmp = reason), by = "Position") |> 
    mutate(ExcludeReason = case_when(
      is.na(ExcludeReason) & is.na(ExcludeReasonTmp) ~ NA_character_,
      is.na(ExcludeReason) ~ ExcludeReasonTmp,
      is.na(ExcludeReasonTmp) ~ ExcludeReason,
      
      !is.na(ExcludeReason) & !is.na(ExcludeReasonTmp) ~ paste0(ExcludeReason, ", ", ExcludeReasonTmp),
      
      TRUE ~ ExcludeReason)) |> 
    select(-ExcludeReasonTmp)
}

####################################################################

# More alleles per position
d_exclude_01_more_entries <- d_tmp_d_helix |> 
  group_by(Position) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  filter(n > 1L) |> 
  select(Position) |> 
  arrange(Position)

d_tmp_d_helix_step1 <- d_tmp_d_helix |> 
  exclude_positions(d_exclude_01_more_entries, "Non-binary")

d_tmp_d_helix_step1 |> count(ExcludeReason)
d_tmp_d_helix_step1 |> distinct(Position, ExcludeReason) |> count(ExcludeReason)
d_tmp_d_helix_step1 |> filter(!is.na(ExcludeReason))

####################################################################

# INDELs
d_exclude_02a_longest_allele_position <- d_tmp_d_helix |> 
  anti_join(d_exclude_01_more_entries, by = "Position") |> 
  filter(LongestAlleleLength > 1) |> 
  select(Position, LongestAlleleLength) |> 
  arrange(Position)

d_tmp_d_helix_step1 |> filter(between(Position, 80, 90))
d_tmp_d_helix_step2 <- d_tmp_d_helix_step1 |> 
  exclude_positions(d_exclude_02a_longest_allele_position |> select(Position), "INDEL")
d_tmp_d_helix_step2 |> filter(between(Position, 80, 90))

d_tmp_d_helix_step2 |> count(ExcludeReason)
d_tmp_d_helix_step2 |> distinct(Position, ExcludeReason) |> count(ExcludeReason)
d_tmp_d_helix_step2 |> filter(!is.na(ExcludeReason))



####################################################################




# INDELs' neigbours
N_NEIGHBOR <- 2
d_exclude_02b_longest_allele_position_neigbors <- d_exclude_02a_longest_allele_position |> 
  mutate(StartPos = Position - N_NEIGHBOR + 1,
         EndPos = Position + LongestAlleleLength + N_NEIGHBOR - 1) |> 
  rowwise() |> 
  mutate(Pos = list(StartPos:EndPos)) |> 
  ungroup() |> 
  select(Position = Pos) |> 
  unnest(Position) |> 
  distinct(Position) |> 
  
  # Only neighbours, not INDELs themselves
  anti_join(d_exclude_02a_longest_allele_position |> select(Position), by = "Position")


d_tmp_d_helix_step2 |> filter(between(Position, 80, 90))
d_tmp_d_helix_step3 <- d_tmp_d_helix_step2 |> 
  exclude_positions(d_exclude_02b_longest_allele_position_neigbors |> select(Position), "INDEL neighbour")
d_tmp_d_helix_step3 |> filter(between(Position, 80, 90))

d_tmp_d_helix_step3 |> count(ExcludeReason)
d_tmp_d_helix_step3 |> distinct(Position, ExcludeReason) |> count(ExcludeReason)
d_tmp_d_helix_step3 |> filter(!is.na(ExcludeReason))


####################################################################

# haplogroups_for_homoplasmic_variants empty
d_exclude_03_no_homoplasmic_variants <- d_tmp_d_helix |> 
  filter(haplogroups_for_homoplasmic_variants == "[]") |> 
  distinct(Position) |> 
  arrange(Position)


d_tmp_d_helix_step3
d_tmp_d_helix_step4 <- d_tmp_d_helix_step3 |> 
  exclude_positions(d_exclude_03_no_homoplasmic_variants |> select(Position), "No homoplasmic variants")
d_tmp_d_helix_step4

d_tmp_d_helix_step4 |> count(ExcludeReason)
d_tmp_d_helix_step4 |> distinct(Position, ExcludeReason) |> count(ExcludeReason)
d_tmp_d_helix_step4 |> filter(!is.na(ExcludeReason))




####################################################################

# seen at least 2 times overall (not in each HG)
d_exclude_05_seen_at_least_twice_tmp <- d_tmp_d_helix_step4 |> 
  #slice(1:100) |> 
  select(Position, 
         A,
         HGHom = haplogroups_for_homoplasmic_variants) |> 
  rowwise() |> 
  mutate(HGHom = list(jsonlite::parse_json(HGHom))) |> 
  select(Position, A, HGHom) |> 
  ungroup() |> 
  rowwise() |> 
  mutate(HGHomtbl = list(as.data.frame(do.call(rbind, lapply(HGHom, unlist))) |> as_tibble())) |>  
  
  ungroup() |> 
  rowwise() |> 
  # We know there are only two alleles
  mutate(Ref = A[[1]],
         Alt = A[[2]]) |> 
  ungroup() |> 
  select(Position, Ref, Alt, HGHomtbl) |> 
  unnest(HGHomtbl) |> 
  rename(HG = V1, 
         n = V2)

d_exclude_05_seen_at_least_twice_tmp2 <- d_exclude_05_seen_at_least_twice_tmp |> 
  group_by(Position) |> 
  summarise(n = sum(as.integer(n)), 
            .groups = "drop") 
d_exclude_05_seen_at_least_twice <- d_exclude_05_seen_at_least_twice_tmp2 |> filter(n < 2)


d_tmp_d_helix_step4
d_tmp_d_helix_step5 <- d_tmp_d_helix_step4 |> 
  exclude_positions(d_exclude_05_seen_at_least_twice |> select(Position), "Not seen twice (2)")
d_tmp_d_helix_step5

d_tmp_d_helix_step5 |> count(ExcludeReason)
d_tmp_d_helix_step5 |> distinct(Position, ExcludeReason) |> count(ExcludeReason)
d_tmp_d_helix_step5 |> filter(!is.na(ExcludeReason))



####################################################################
####################################################################

d_tmp_d_helix_step_final <- d_tmp_d_helix_step5

####################################################################
####################################################################

d_helix_info <- d_tmp_d_helix_step_final |> 
  select(-LongestAllele, -LongestAlleleLength)
d_helix_info

d_helix_info |> 
  count(ExcludeReason)

####################################################################

d_helix_HG_long_raw <- d_tmp_d_helix_step_final |> 
  filter(is.na(ExcludeReason)) |> 
  #slice(1:100) |> 
  select(Position, 
         A,
         HGHom = haplogroups_for_homoplasmic_variants) |> 
  rowwise() |> 
  mutate(HGHom = list(jsonlite::parse_json(HGHom))) |> 
  select(Position, A, HGHom) |> 
  ungroup() |> 
  rowwise() |> 
  mutate(HGHomtbl = list(as.data.frame(do.call(rbind, lapply(HGHom, unlist))) |> as_tibble())) |>  
  
  ungroup() |> 
  rowwise() |> 
  # We know there are only two alleles
  mutate(Ref = A[[1]],
         Alt = A[[2]]) |> 
  ungroup() |> 
  select(Position, Ref, Alt, HGHomtbl) |> 
  unnest(HGHomtbl) |> 
  rename(HG = V1, 
         n = V2) |> 
  
  rename_TLHGs() |> 
  
  mutate(n = as.integer(n)) |> 
  group_by(Position, Ref, Alt, HG) |> 
  summarise(n = sum(n), .groups = "drop") |> 
  rename(TLHG = HG) |> 
  left_join(d_TLHG_n |> rename(N_HG = N), by = "TLHG") 



d_helix_long <- d_helix_HG_long_raw |> 
  mutate(n_Alt = as.integer(n),
         n_Ref = N_HG - n_Alt) |> 
  select(-n) |> 
  mutate(p_Alt = n_Alt / N_HG,
         p_Ref = n_Ref / N_HG) |> 
  ungroup() 

########################################

stopifnot(d_helix_info |> filter(is.na(ExcludeReason)) |> nrow()
          ==
            d_helix_long |> distinct(Position) |> nrow()
)

########################################

d_helix_TLHG_freq <- d_TLHG_n 

d_helix_positioninfo <- d_helix_info |> 
  select(Position, ExcludeReason)

d_helix_SNV_freq_long <- d_helix_long |> 
  select(Position, 
         Ref, Alt, 
         TLHG, 
         n_Ref, n_Alt)
  
########################################


usethis::use_data(d_helix_TLHG_freq, overwrite = TRUE)
usethis::use_data(d_helix_positioninfo, overwrite = TRUE)
usethis::use_data(d_helix_SNV_freq_long, overwrite = TRUE)


