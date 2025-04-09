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
#' @source Bolze et al. (2020). "A catalog of homoplasmic and heteroplasmic mitochondrial DNA variants in humans" (
#' <https://www.biorxiv.org/content/10.1101/798264v3> / <https://doi.org/10.1101/798264>)
"d_helix_TLHG_freq"


#' HelixMTdb raw data
#' 
#' Only information about homoplasmic variants.
#'
#' @format ## `d_helix`
#' A tibble with 30 rows and 2 columns:
#' \describe{
#'   \item{Position}{mtDNA position.}
#'   \item{Ref}{Reference base.}
#'   \item{Alt}{Variant base.}
#'   \item{HGHom}{Variant counts in different TLHGs.}
#'   \item{HGHomN}{Total from `HGHom`.}
#'   \item{ExcludeReason}{Exclusion reason, `NA` if included in `d_helix_refined_long`.}
#' }
#' @source Bolze et al. (2020). "A catalog of homoplasmic and heteroplasmic mitochondrial DNA variants in humans" (
#' <https://www.biorxiv.org/content/10.1101/798264v3> / <https://doi.org/10.1101/798264>)
"d_helix"

#' HelixMTdb single nucleotide variation frequency information
#' 
#' Information for each single nucleotide variation (SNV) position on 
#' frequencies in each top-level haplogroup (TLHG) for the included positions 
#' (cf. `d_helix`). Note, only homoplasmic variants included.
#'
#' @format ## `d_helix_refined_long`
#' A tibble with 28,751 rows and 6 columns:
#' \describe{
#'   \item{Position}{mtDNA position.}
#'   \item{Ref}{Revised Cambridge Reference Sequence (rCRS) base.}
#'   \item{TLHG}{Top-level haplogroup.}
#'   \item{n}{Number of individuals in the TLHG with the `Base`.}
#'   \item{Base}{The `Base` that `n` is a frequency of.}
#'   \item{Type}{The `Base` type (either reference or variant).}
#'   \item{N_TLHG}{Number of individuals in the TLHG, cf. `d_helix_TLHG_freq`.}
#' }
#' @source Bolze et al. (2020). "A catalog of homoplasmic and heteroplasmic mitochondrial DNA variants in humans" (
#' <https://www.biorxiv.org/content/10.1101/798264v3> / <https://doi.org/10.1101/798264>)
"d_helix_refined_long"
