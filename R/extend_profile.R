#' Get positions from variants
#' 
#' @param x Vector of variants
#' 
#' @export
positions_from_variants <- function(x) {
  gsub("^([0-9]+).*$", "\\1", x) |> as.integer()
}

#' Get positions from variants
#' 
#' @param x Vector of variants
#' 
#' @export
bases_from_variants <- function(x) {
  gsub("^[0-9]+(.*)$", "\\1", x) 
}

#' Parse range
#' 
#' @param range_str E.g. "1-16569"
#' 
#' @examples
#' parse_range("1-16569")
#' parse_range("1-1,2")
#' parse_range("1-10,222")
#' parse_range("16024-576")
#' 
#' @export
parse_range <- function(range_str) {
  clean_str <- gsub(" ", "", range_str, fixed = TRUE)
  
  # Validate characters: only digits, commas, and hyphens allowed
  if (!grepl("^[0-9,-]*$", clean_str)) {
    return(NA_integer_)
  }
  
  parts <- unlist(strsplit(clean_str, ","))
  result <- c()
  
  for (part in parts) {
    if (grepl("-", part)) {
      bounds <- as.integer(unlist(strsplit(part, "-")))
      if (length(bounds) != 2 || any(is.na(bounds))) {
        return(NA_integer_)
      }
      
      if (bounds[1] > bounds[2]) {
        result <- c(result, 
                    seq(bounds[1], 16569),
                    seq(1, bounds[2]))
      } else {
        result <- c(result, seq(bounds[1], bounds[2]))
      }
    } else {
      num <- as.integer(part)
      if (is.na(num)) {
        return(NA_integer_)
      }
      result <- c(result, num)
    }
  }
  
  return(result)
}



#' Extend profile to all positions with frequency information
#' 
#' Typically, either Helix (`d_helix_refined_long`) or 
#' gnomAD (`d_gnomAD_refined_long`).
#' 
#' @param variants Vector of variants
#' @param tlhg Top-level haplogroup
#' @param range Positions range (default = 1:16569)
#' @param d_SNV_long Typically, `d_helix_refined_long` or `d_gnomAD_refined_long`
#' 
#' @importFrom dplyr select filter pull anti_join semi_join left_join summarise group_by mutate
#' @importFrom tibble tibble
#' @export
extend_profile <- function(variants, 
                           tlhg, 
                           range = 1:16569, 
                           d_SNV_long = d_helix_refined_long) {
  # variants <- c("21AT", "263G", "9150G")
  # tlhg <- "H"
  
  # variants <- c("93G", "263G", "315.1C", "477C", "16519C")
  # tlhg <- "H"
  # range <- parse_range("16024-576")
  
  tlhg <- unique(tlhg)
  stopifnot(length(tlhg) == 1L)
  stopifnot(all(range >= 1L & range <= 16569L))
  
  pos <- positions_from_variants(variants)
  base <- bases_from_variants(variants)
  
  d_profile <- tibble::tibble(Position = pos, 
                              Base = base, 
                              Variant = variants)
  
  d_tmp_SNP_HG <- d_SNV_long |> 
    dplyr::filter(TLHG == tlhg) 
  
  # Not found
  if (nrow(d_tmp_SNP_HG) <= 0L) {
    return(NULL)
  }

  # Remove positions not in range:
  d_tmp_range <- tibble(Position = range)
  d_variants_ignored_range <- d_profile |> 
    dplyr::anti_join(d_tmp_range, by = "Position")
  
  # Remove positions not in d_SNV:
  d_variants_ignored <- d_profile |> 
    dplyr::anti_join(d_tmp_SNP_HG, by = "Position")

  d_only_SNV <- d_profile |>
    dplyr::semi_join(d_tmp_range, by = "Position") |> 
    dplyr::semi_join(d_tmp_SNP_HG, by = "Position")
  
  # Extend by ALL positions:
  d_all_SNV_prob <- d_tmp_SNP_HG |> 
    dplyr::semi_join(d_tmp_range, by = "Position") |> 
    dplyr::left_join(d_only_SNV |> dplyr::select(Position, Base, Variant), by = c("Position", "Base")) |> 
    dplyr::select(-TLHG) |> 
    dplyr::group_by(Position) |> 
    dplyr::summarise(
      Ref = Ref[1L],
      var_present = any(!is.na(Variant)),
      idx = ifelse(var_present, 
                   which(!is.na(Variant)), 
                   which(Type == "Ref"))[1L],
      n = n[idx],
      Base = Base[idx],
      Type = Type[idx],
      N_TLHG = N_TLHG[idx],
      
      .groups = "drop") |> 
    dplyr::mutate(p_Base = n / N_TLHG) |> 
    dplyr::select(-var_present, -idx) |> 
    dplyr::select(Position, Ref, Profile = Base, BaseType = Type, 
                  N_TLHG, n_Base = n, p_Base)
  
  #d_all_SNV_prob
  
  return(list(
    d_profile_ext = d_all_SNV_prob,
    d_variants_ignored_range = d_variants_ignored_range,
    d_variants_ignored = d_variants_ignored
  ))
}