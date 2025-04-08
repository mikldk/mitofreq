## code to prepare Helix datasets

library(tidyverse)
library(readxl)
library(here)
library(stringi)
library(jsonlite)

################################################################################

################################################################################

helix_db_file <- tempfile()
download.file(url = "https://s3.amazonaws.com/helix-research-public/mito/HelixMTdb_20200327.tsv", 
                          destfile = helix_db_file)


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
  rename(TLHG = HG) |> 
  rename_TLHGs() |> 
  group_by(TLHG) |> 
  summarise(N = sum(N), .groups = "drop") 



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
  ungroup() 


####################################################################

d_tmp_d_helix_parsed <- d_tmp_d_helix |> 
  rename(HGHom = haplogroups_for_homoplasmic_variants) |> 
  rowwise() |> 
  mutate(HGHom = list(jsonlite::parse_json(HGHom))) |> 
  #mutate(HGHomtbl = list(as.data.frame(do.call(rbind, lapply(HGHom, unlist))) |> set_names(c("TLHG", "n")) |> as_tibble())) |>
  mutate(HGHomtbl = list({
    x <- as.data.frame(do.call(rbind, lapply(HGHom, unlist))) |> as_tibble()
    
    if (nrow(x) > 0L) {
      colnames(x) <- c("TLHG", "n")
      x <- x |> 
        mutate(n = as.integer(n))
    }
    x
    })) |>  
  ungroup() |> 
  select(-HGHom, -alleles, -locus) |> 
  rename(HGHom = HGHomtbl) 

d_tmp_d_helix_parsed
d_tmp_d_helix_parsed |> slice(1:4) |> pull(HGHom)

# Check that counts_hom is correct
d_tmp_d_helix_parsed_n <- d_tmp_d_helix_parsed |> 
  mutate(n = lapply(HGHom, \(x) {
    if (is.null(x) || nrow(x) == 0L) {
      return(0L)
    }
    return(x |> pull(n) |> sum())
  }) |> unlist())
d_tmp_d_helix_parsed_n
d_tmp_d_helix_parsed_n |> filter(counts_hom != n)


###
d_tmp_d_helix_parsed |> 
  rowwise() |> 
  mutate(n_A = length(A)) |> 
  ungroup() |> 
  pull(n_A) |> 
  table()

d_tmp_d_helix_parsed |> 
  filter(lengths(A) == 3) 


#' remove all alleles with not 2 alleles (they all have counts_hom == 0)
d_helix <- d_tmp_d_helix_parsed |> 
  filter(lengths(A) == 2) |> 
  rowwise() |> 
  mutate(Ref = A[[1L]], Alt = A[[2L]]) |> 
  ungroup() |> 
  select(-A) |> 
  select(Position, Ref, Alt, HGHom, HGHomN = counts_hom)
d_helix

################################################################################


#' First, calculate base frequencies
#' 
#' We only take positions with one reference allele (or else we do not know
#' the count of each reference allele)

rmd_01_one_reference <- d_helix |> 
  group_by(Position) |> 
  mutate(n_ref = Ref |> unique() |> length()) |> 
  ungroup() |> 
  filter(n_ref > 1L) |> 
  distinct(Position)


# Only saves the first reason for removal
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
      TRUE ~ ExcludeReason)) |> 
    select(-ExcludeReasonTmp)
  
  # d |> 
  #   left_join(d_pos |> mutate(ExcludeReasonTmp = reason), by = "Position") |> 
  #   mutate(ExcludeReason = case_when(
  #     is.na(ExcludeReason) & is.na(ExcludeReasonTmp) ~ NA_character_,
  #     is.na(ExcludeReason) ~ ExcludeReasonTmp,
  #     is.na(ExcludeReasonTmp) ~ ExcludeReason,
  #     
  #     !is.na(ExcludeReason) & !is.na(ExcludeReasonTmp) ~ paste0(ExcludeReason, ", ", ExcludeReasonTmp),
  #     
  #     TRUE ~ ExcludeReason)) |> 
  #   select(-ExcludeReasonTmp)
}


d_helix <- d_helix |> 
  exclude_positions(rmd_01_one_reference, "More than one reference")

d_helix |> distinct(Position) |> nrow()
d_helix |> distinct(Position, ExcludeReason) |> count(ExcludeReason)

d_helix_refined_ref_n <- d_helix |> 
  filter(is.na(ExcludeReason)) |> 
  unnest(HGHom) |> 
  rename_TLHGs() |> 
  group_by(Position, Ref, TLHG) |> 
  summarise(n_Alt = sum(n), 
            .groups = "drop") |> 
  left_join(d_helix_TLHG_freq |> rename(N_TLHG = N), by = "TLHG") |> 
  mutate(n_Ref = N_TLHG - n_Alt) |> 
  select(Position, Ref, TLHG, N_TLHG, n_Ref)
d_helix_refined_ref_n
d_helix_refined_ref_n |> filter(is.na(N_TLHG))



d_helix_refined_var_n <- d_helix |> 
  filter(is.na(ExcludeReason)) |> 
  unnest(HGHom) |> 
  rename_TLHGs() |> 
  group_by(Position, TLHG, Ref, Alt) |> 
  summarise(n_Alt = sum(n), 
            .groups = "drop")
d_helix_refined_var_n


d_helix_refined_long <- 
  bind_rows(
    d_helix_refined_ref_n |> select(Position, Ref, TLHG, n = n_Ref) |> mutate(Base = Ref) |> mutate(Type = "Ref"),
    d_helix_refined_var_n |> select(Position, Ref, Base = Alt, TLHG, n = n_Alt) |> mutate(Type = "Alt")
  ) |> 
  arrange(Position, Ref, TLHG, Type, Base) |> 
  left_join(d_helix_TLHG_freq |> rename(N_TLHG = N), by = "TLHG") 
d_helix_refined_long



n_pos_prob <- d_helix_refined_long |> 
  group_by(Position, TLHG, Ref) |> 
  summarise(n = sum(n),
            N_TLHG = unique(N_TLHG),
            .groups = "drop") |> 
  filter(N_TLHG != n) |> 
  nrow()
stopifnot(n_pos_prob == 0L)

################################################################################
#' Other exclusions:

exclude_positions_ref_base <- function(d, d_pos, reason) {
  if (!("ExcludeReason" %in% colnames(d))) {
    d <- d |> 
      mutate(ExcludeReason = NA_character_)
  }
  
  d |> 
    left_join(d_pos |> mutate(ExcludeReasonTmp = reason), by = c("Position", "Ref", "Alt" = "Base")) |> 
    mutate(ExcludeReason = case_when(
      is.na(ExcludeReason) & is.na(ExcludeReasonTmp) ~ NA_character_,
      is.na(ExcludeReason) ~ ExcludeReasonTmp,
      TRUE ~ ExcludeReason)) |> 
    select(-ExcludeReasonTmp)
}

################################################################################

#' 1)
#' Variant must be seen at least twice (not within TLHG)
rmd_02_at_least_twice <- d_helix_refined_long |> 
  group_by(Position, Ref, Base) |> 
  summarise(n = sum(n), 
            .groups = "drop") |> 
  filter(n <= 1L)

d_helix_refined_long |> 
  semi_join(rmd_02_at_least_twice, by = c("Position", "Ref", "Base"))
d_helix_refined_long <- d_helix_refined_long |> 
  anti_join(rmd_02_at_least_twice, by = c("Position", "Ref", "Base"))

d_helix2 <- d_helix |> 
  exclude_positions_ref_base(rmd_02_at_least_twice |> select(-n), "Variant only seen once")

d_helix2 |> distinct(Position) |> nrow()
d_helix2 |> distinct(Position, ExcludeReason) |> count(ExcludeReason)
d_helix |> distinct(Position, ExcludeReason) |> count(ExcludeReason)

d_helix <- d_helix2
################################################################################

d_helix_TLHG_freq <- d_TLHG_n 
usethis::use_data(d_helix_TLHG_freq, overwrite = TRUE)

#usethis::use_data(d_helix, overwrite = TRUE)

usethis::use_data(d_helix, overwrite = TRUE)
usethis::use_data(d_helix_refined_long, overwrite = TRUE)


