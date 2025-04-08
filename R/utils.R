#' Rename TLHG
#' 
#' @param d dataset with column named HG that will be renamed
rename_TLHGs <- function(d) {
  d |> 
    mutate(TLHG = case_when(
      TLHG == "X + S" ~ "X",
      TLHG == "L4 + L5 + L6" ~ "L4-6",
      TLHG == "L4" ~ "L4-6",
      TLHG == "L5" ~ "L4-6",
      TLHG == "L6" ~ "L4-6",
      TLHG == "R" | TLHG == "B" ~ "R/B",
      TRUE ~ TLHG
    )) 
}
