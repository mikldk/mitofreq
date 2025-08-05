#' Revised Cambridge Reference Sequence (rCRS)
#' 
#' 
#' @examples
#' subset(d_rCRS, !(Ref %in% c("A", "T", "G", "C")))
#' 
#' @format ## `d_rCRS`
#' A tibble with 16,569 rows and 2 columns:
#' \describe{
#'   \item{Position}{mtDNA position.}
#'   \item{Ref}{Base according to rCRS.}
#' }
#' @source <https://www.ncbi.nlm.nih.gov/nuccore/NC_012920.1?report=fasta>
"d_rCRS"