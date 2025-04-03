#' Extend profile to all Helix positions
#' 
#' @param variants Vector of variants
#' @param tlhg Top-level haplogroup
#' 
#' @export
extend_profile_to_helix_positions <- function(variants, tlhg) {
  # variants <- c("263G", "9150G")
  # tlhg <- "H"
  tlhg <- unique(tlhg)
  stopifnot(length(tlhg) == 1L)
  
  pos <- gsub("^([0-9]+).*$", "\\1", variants) |> as.integer()
  base <- gsub("^[0-9]+(.*)$", "\\1", variants) 
  
  d_profile <- tibble(Position = pos, 
                      Base = base, 
                      Variant = variants)
  
  d_tmp_helix_HG <- mitofreq::d_helix_SNV_freq_long |> 
    filter(TLHG == tlhg) 
  
  # Not found
  if (nrow(d_tmp_helix_HG) <= 0L) {
    return(NULL)
  }
  
  d_TLHG_info <- mitofreq::d_helix_TLHG_freq |> 
    filter(TLHG == tlhg) 
  if (nrow(d_TLHG_info) != 1L) {
    return(NULL)
  }
  TLHG_N <- d_TLHG_info |> pull(N)
  
  # Remove positions not in Helix:
  d_variants_ignored <- d_profile |> 
    anti_join(d_tmp_helix_HG, by = "Position")
  d_only_helix <- d_profile |> 
    semi_join(d_tmp_helix_HG, by = "Position")
  
  # Extend by ALL Helix positions:
  d_all_helix <- d_tmp_helix_HG |> 
    left_join(d_only_helix |> select(Position, Base, Variant), by = "Position")
  
  #d_all_helix |> print(n = Inf)
  #d_all_helix |> filter(!is.na(Base))
  
  d_all_helix_SNV_prob <- d_all_helix |> 
    mutate(Base = ifelse(is.na(Base), Ref, Base)) |> 
    mutate(n_SNV = case_when(
      Base == Ref ~ n_Ref,
      Base == Alt ~ n_Alt,
      TRUE ~ NA_real_
    )) |> 
    mutate(p_SNV = n_SNV / TLHG_N)
  
  return(list(
    d_profile_ext = d_all_helix_SNV_prob,
    d_variants_ignored = d_variants_ignored
  ))
}