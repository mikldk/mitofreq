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
