## code to prepare gnomAD datasets

library(tidyverse)
library(readxl)
library(here)
library(stringi)
library(jsonlite)
library(vcfR)

################################################################################

gnomad_file <- tempfile()
download.file(url = "https://storage.googleapis.com/gcp-public-data--gnomad/release/3.1/vcf/genomes/gnomad.genomes.v3.1.sites.chrM.vcf.bgz", 
              destfile = gnomad_file)

################################################################################
gnomAD_org <- vcfR::read.vcfR(gnomad_file, verbose = FALSE)
gnomAD_org
################################################################################

gnomAD_org@fix[1:3, 1:5]
pos <- gnomAD_org@fix[, 2] |> as.integer()
range(pos)


#' AN: "Overall allele number (number of samples with non-missing genotype)"
#' AC_hom: "Allele count restricted to variants with a heteroplasmy level >= 0.95"
#' AF_hom: "Allele frequency restricted to variants with a heteroplasmy level >= 0.95"
#' hap_AN: "List of overall allele number for each haplogroup, haplogroup order: ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'HV', 'I', 'J', 'K', 'L0', 'L1', 'L2', 'L3', 'L4', 'L5', 'M', 'N', 'P', 'R', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']"
#' hap_AC_hom: "List of AC_hom for each haplogroup, haplogroup order: ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'HV', 'I', 'J', 'K', 'L0', 'L1', 'L2', 'L3', 'L4', 'L5', 'M', 'N', 'P', 'R', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']"
#' pop_AN: "List of overall allele number for each population, population order: ['afr', 'ami', 'amr', 'asj', 'eas', 'fin', 'nfe', 'oth', 'sas', 'mid']"
#' pop_AC_hom: "List of AC_hom for each population, population order: ['afr', 'ami', 'amr', 'asj', 'eas', 'fin', 'nfe', 'oth', 'sas', 'mid']"

queryMETA(gnomAD_org)

lst_gnomAD_info <- lapply(c("AN", "AC_hom", "AF_hom", "hap_AN", "hap_AC_hom", "pop_AN", "pop_AC_hom"), \(x) {
  tibble(V = vcfR::extract.info(gnomAD_org, x)) |> 
    rename({{ x }} := V)
})

d_gnomAD_org <- bind_cols(lst_gnomAD_info) |> 
  mutate(Pos = pos, 
         Ref = gnomAD_org@fix[, 4], 
         Alt = gnomAD_org@fix[, 5]) |> 
  select(Pos, Ref, Alt, everything())

d_gnomAD_raw <- d_gnomAD_org |>
  mutate(hap_AN = strsplit(hap_AN, "|", fixed = TRUE)) |> 
  mutate(hap_AC_hom = strsplit(hap_AC_hom, "|", fixed = TRUE)) |> 
  mutate(pop_AN = strsplit(pop_AN, "|", fixed = TRUE)) |>
  mutate(pop_AC_hom = strsplit(pop_AC_hom, "|", fixed = TRUE)) |> 
  mutate(hap = list(c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'HV', 'I', 'J', 'K', 'L0', 'L1', 'L2', 'L3', 'L4', 'L5', 'M', 'N', 'P', 'R', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'))) |> 
  mutate(pop = list(c('afr', 'ami', 'amr', 'asj', 'eas', 'fin', 'nfe', 'oth', 'sas', 'mid')))

################################################################################
################################################################################

#' AN: "Overall allele number (number of samples with non-missing genotype)"
#' AC_hom: "Allele count restricted to variants with a heteroplasmy level >= 0.95"
#' AF_hom: "Allele frequency restricted to variants with a heteroplasmy level >= 0.95"
#' hap_AN: "List of overall allele number for each haplogroup, haplogroup order: ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'HV', 'I', 'J', 'K', 'L0', 'L1', 'L2', 'L3', 'L4', 'L5', 'M', 'N', 'P', 'R', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']"
#' hap_AC_hom: "List of AC_hom for each haplogroup, haplogroup order: ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'HV', 'I', 'J', 'K', 'L0', 'L1', 'L2', 'L3', 'L4', 'L5', 'M', 'N', 'P', 'R', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']"
#' pop_AN: "List of overall allele number for each population, population order: ['afr', 'ami', 'amr', 'asj', 'eas', 'fin', 'nfe', 'oth', 'sas', 'mid']"
#' pop_AC_hom: "List of AC_hom for each population, population order: ['afr', 'ami', 'amr', 'asj', 'eas', 'fin', 'nfe', 'oth', 'sas', 'mid']"

d_gnomAD_raw |> slice(1:3) 
d_gnomAD_raw2 <- d_gnomAD_raw |> 
  select(Position = Pos,
         Ref,
         Alt,
         Ntot = AN,
         HGHomN = AC_hom,
         N_TLHG = hap_AN, 
         n = hap_AC_hom,
         TLHG = hap) |> 
  mutate(Ntot = as.integer(Ntot),
         HGHomN = as.integer(HGHomN),
         N_TLHG = lapply(N_TLHG, as.integer),
         n = lapply(n, as.integer))
d_gnomAD_raw2
d_gnomAD_raw2 |> rowwise() |> mutate(Ntot2 = sum(N_TLHG)) |> filter(Ntot != Ntot2)

d_gnomAD <- d_gnomAD_raw2 |> 
  select(-Ntot)
d_gnomAD

d_gnomAD |> filter(Position == 42)

#' First, calculate base frequencies
#' 
#' We only take positions with one reference allele (or else we do not know
#' the count of each reference allele)

rmd_01_one_reference <- d_gnomAD |> 
  group_by(Position) |> 
  mutate(n_ref = Ref |> unique() |> length()) |> 
  ungroup() |> 
  filter(n_ref > 1L) |> 
  distinct(Position)
rmd_01_one_reference
d_gnomAD |> filter(Position == 52)




d_gnomAD <- d_gnomAD |> 
  mitofreq:::exclude_positions(rmd_01_one_reference, "More than one reference")
d_gnomAD |> filter(Position == 52)

################################################################################

# No variants:
  

rmd_02_no_variants <- d_gnomAD |> 
  group_by(Position) |> 
  summarise(HGHomN = sum(HGHomN),
            .groups = "drop") |> 
  filter(HGHomN == 0L) |> 
  distinct(Position)
rmd_02_no_variants
d_gnomAD |> filter(Position == 44) |> pull(n)

d_gnomAD <- d_gnomAD |> 
  mitofreq:::exclude_positions(rmd_02_no_variants, "No variants")
d_gnomAD |> filter(Position == 44)

################################################################################


d_gnomAD |> 
  filter(is.na(ExcludeReason)) |> 
  distinct(Position, Ref) |> 
  count(Position) |> 
  count(n)

d_gnomAD |> 
  filter(is.na(ExcludeReason)) |> 
  distinct(Position, Ref) |> 
  count(Position) |> 
  count(n)

d_gnomAD |> distinct(Position) |> nrow()
d_gnomAD |> distinct(Position, ExcludeReason) |> count(ExcludeReason)


d_gnomAD_refined_unnested <- d_gnomAD |> 
  filter(is.na(ExcludeReason)) |> 
  select(-ExcludeReason) |> 
  unnest(c(TLHG, N_TLHG, n)) |> 
  select(-HGHomN) |> 
  rename_TLHGs() |> 
  group_by(Position, Ref, Alt, TLHG) |> 
  summarise(N_TLHG = sum(N_TLHG),
            n = sum(n), 
            .groups = "drop")

d_gnomAD_refined_unnested |> filter(Position == 16290, TLHG == "A") |> print(n = Inf)

d_gnomAD |> filter(Position == 44)
d_gnomAD_refined_unnested |> filter(Position == 44) |> print(n = Inf)


d_gnomAD_refined_ref_n <- d_gnomAD_refined_unnested |> 
  group_by(Position, Ref, TLHG) |> 
  summarise(n_Alt = sum(n),  
            N_TLHG = sum(N_TLHG), 
            .groups = "drop") |> 
  mutate(n_Ref = N_TLHG - n_Alt) |> 
  select(Position, Ref, TLHG, N_TLHG, n_Ref)
d_gnomAD_refined_ref_n |> filter(Position == 16290, TLHG == "A") |> print(n = Inf)

d_gnomAD_refined_ref_n
d_gnomAD_refined_ref_n |> filter(is.na(N_TLHG))



d_gnomAD_refined_var_n <- d_gnomAD_refined_unnested |> 
  select(-N_TLHG) |> 
  group_by(Position, TLHG, Ref, Alt) |> 
  summarise(n_Alt = sum(n), 
            .groups = "drop") |> 
  left_join(d_gnomAD_refined_ref_n, by = c("Position", "TLHG", "Ref")) 
d_gnomAD_refined_var_n
d_gnomAD_refined_var_n |> filter(Position == 16290, TLHG == "A") |> print(n = Inf)

d_gnomAD_refined_long <- 
  bind_rows(
    d_gnomAD_refined_ref_n |> select(Position, Ref, TLHG, n = n_Ref, N_TLHG) |> mutate(Base = Ref) |> mutate(Type = "Ref"),
    d_gnomAD_refined_var_n |> select(Position, Ref, Base = Alt, TLHG, n = n_Alt, N_TLHG) |> mutate(Type = "Alt")
  ) |> 
  arrange(Position, Ref, TLHG, Type, Base) |> 
  select(Position, Ref, TLHG, n, Base, Type, N_TLHG) |> 
  filter(n > 0)

d_gnomAD_refined_long
d_gnomAD_refined_long |> filter(Position == 16290, TLHG == "A") |> print(n = Inf)

d_gnomAD_refined_long |> filter(Position == 10, TLHG == "H")
d_helix_refined_long |> filter(Position == 10, TLHG == "H")


n_pos_prob <- d_gnomAD_refined_long |> 
  group_by(Position, TLHG, Ref) |> 
  summarise(n = sum(n),
            N_TLHG = unique(N_TLHG),
            .groups = "drop") |> 
  filter(N_TLHG != n)
n_pos_prob
stopifnot(n_pos_prob |> nrow() == 0L)


n_pos_prob <- d_gnomAD_refined_long |>
  mutate(p = n / N_TLHG) |> 
  group_by(Position, TLHG) |> 
  summarise(p = sum(p),
            .groups = "drop") |> 
  filter(abs(p - 1) > 1e-4) |> 
  arrange(desc(abs(p - 1)))# |> filter(TLHG == "A")
n_pos_prob
stopifnot(n_pos_prob |> nrow() == 0L)


################################################################################
#' Other exclusions:


################################################################################

#' 1)
#' Variant must be seen at least twice (not within TLHG)
rmd_03_at_least_twice <- d_gnomAD_refined_long |> 
  filter(Type == "Alt") |> 
  group_by(Position, Ref, Base) |> 
  summarise(n = sum(n), 
            .groups = "drop") |> 
  filter(n <= 1L) 
rmd_03_at_least_twice
rmd_03_at_least_twice |> filter(Position == 15)
# All variants at position
rmd_03_at_least_twice <- rmd_03_at_least_twice |> distinct(Position)

rmd_03_at_least_twice
d_gnomAD_refined_long |> filter(Position == 15, Type == "Alt")

d_gnomAD_refined_long |> 
  semi_join(rmd_03_at_least_twice, by = c("Position"))
d_gnomAD_refined_long <- d_gnomAD_refined_long |> 
  anti_join(rmd_03_at_least_twice, by = c("Position"))

d_gnomAD2 <- d_gnomAD |> 
  mitofreq:::exclude_positions(rmd_03_at_least_twice, "Variant only seen once")

d_gnomAD2 |> distinct(Position) |> nrow()
d_gnomAD2 |> distinct(Position, ExcludeReason) |> count(ExcludeReason)
d_gnomAD |> distinct(Position, ExcludeReason) |> count(ExcludeReason)

################################################################################

# d_gnomAD_TLHG_freq_raw <- d_gnomAD_raw |> 
#   select(Position = Pos,
#          Ref,
#          Alt,
#          #Pop = pop,
#          #N_pop = pop_AN,
#          TLHG = hap,
#          N_TLHG = hap_AN) |> 
#   mutate(#N_pop = lapply(N_pop, as.integer),
#          N_TLHG = lapply(N_TLHG, as.integer))

d_gnomAD_TLHG_freq <- d_gnomAD |> 
  select(TLHG, N_TLHG) |> 
  unnest(c(TLHG, N_TLHG)) |> 
  rename_TLHGs() |> 
  group_by(TLHG) |> 
  summarise(N_TLHG = mean(N_TLHG), 
            .groups = "drop")
d_gnomAD_TLHG_freq |> print(n = Inf)
d_gnomAD_TLHG_freq

d_gnomAD |> slice(1) |> pull(N_TLHG)

################################################################################

usethis::use_data(d_gnomAD_TLHG_freq, overwrite = TRUE)
usethis::use_data(d_gnomAD, overwrite = TRUE)
usethis::use_data(d_gnomAD_refined_long, overwrite = TRUE)

################################################################################