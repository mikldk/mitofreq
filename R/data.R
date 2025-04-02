#' HelixMTdb TLHG distribution
#' 
#' Counts of individuals in top-level haplogroups from HelixMTdb.
#'
#' @format ## `d_helix_TLHG_freq`
#' A tibble with 30 rows and 2 columns:
#' \describe{
#'   \item{TLHG}{Top-level haplogroup.}
#'   \item{N}{Number of individuals in the top-level haplogroup.}
#' }
#' @source "A catalog of homoplasmic and heteroplasmic mitochondrial DNA variants in humans" (
#' <https://www.biorxiv.org/content/10.1101/798264v3> / <https://doi.org/10.1101/798264>)
"d_helix_TLHG_freq"

#' HelixMTdb position information
#' 
#' Information for each position if excluded (why) or not.
#'
#' @format ## `d_helix_positioninfo`
#' A tibble with 14,324 rows and 2 columns:
#' \describe{
#'   \item{Position}{mtDNA position.}
#'   \item{ExcludeReason}{Reason for excluded Position, `NA` if not excluded (i.e. included).}
#' }
#' @source "A catalog of homoplasmic and heteroplasmic mitochondrial DNA variants in humans" (
#' <https://www.biorxiv.org/content/10.1101/798264v3> / <https://doi.org/10.1101/798264>)
"d_helix_positioninfo"


#' HelixMTdb single nucleotide variation frequency information
#' 
#' Information for each single nucleotide variation (SNV) position on 
#' frequencies in each top-level haplogroup (TLHG) for the included positions 
#' (only binary SNVs included). 
#'
#' @format ## `d_helix_SNV_freq_long`
#' A tibble with 28,751 rows and 6 columns:
#' \describe{
#'   \item{Position}{mtDNA position.}
#'   \item{Ref}{Revised Cambridge Reference Sequence (rCRS) base.}
#'   \item{Alt}{Alternative base.}
#'   \item{TLHG}{Top-level haplogroup}
#'   \item{n_Ref}{Number of individuals in the top-level haplogroup with the reference rCRS allele.}
#'   \item{n_Alt}{Number of individuals in the top-level haplogroup with the alternative allele.}
#' }
#' @source "A catalog of homoplasmic and heteroplasmic mitochondrial DNA variants in humans" (
#' <https://www.biorxiv.org/content/10.1101/798264v3> / <https://doi.org/10.1101/798264>)
"d_helix_SNV_freq_long"

