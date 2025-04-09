#' GnomAD v. 3.1 TLHG distribution
#' 
#' Counts of individuals in top-level haplogroups, averages accross mitogenome 
#' (as a TLHG count is given at each position).
#'
#' @format ## `d_gnomAD_TLHG_freq`
#' A tibble with 30 rows and 2 columns:
#' \describe{
#'   \item{TLHG}{Top-level haplogroup.}
#'   \item{N}{Number of individuals in the top-level haplogroup.}
#' }
#' @source <https://gnomad.broadinstitute.org/news/2020-11-gnomad-v3-1-mitochondrial-dna-variants/>
"d_gnomAD_TLHG_freq"


#' GnomAD raw data
#' 
#' Only information about homoplasmic variants.
#'
#' @format ## `d_gnomAD`
#' A tibble with 30 rows and 2 columns:
#' \describe{
#'   \item{Position}{mtDNA position.}
#'   \item{Ref}{Reference base.}
#'   \item{Alt}{Variant base.}
#'   \item{HGHomN}{Total from `n`.}
#'   \item{N_TLHG}{Number of individuals in the top-level haplogroup.}
#'   \item{n}{Variant counts in different TLHGs.}
#'   \item{TLHG}{TLHGs for the other columns.}
#'   \item{ExcludeReason}{Exclusion reason, `NA` if included in `d_gnomAD_refined_long`.}
#' }
#' @source <https://gnomad.broadinstitute.org/news/2020-11-gnomad-v3-1-mitochondrial-dna-variants/>
"d_gnomAD"

#' GnomAD single nucleotide variation frequency information
#' 
#' Information for each single nucleotide variation (SNV) position on 
#' frequencies in each top-level haplogroup (TLHG) for the included positions 
#' (cf. `d_gnomAD`). Note, only homoplasmic variants included.
#'
#' @format ## `d_gnomAD_refined_long`
#' A tibble with 28,751 rows and 6 columns:
#' \describe{
#'   \item{Position}{mtDNA position.}
#'   \item{Ref}{Revised Cambridge Reference Sequence (rCRS) base.}
#'   \item{TLHG}{Top-level haplogroup.}
#'   \item{n}{Number of individuals in the TLHG with the `Base`.}
#'   \item{Base}{The `Base` that `n` is a frequency of.}
#'   \item{Type}{The `Base` type (either reference or variant).}
#'   \item{N_TLHG}{Number of individuals in the TLHG (note, position specific).}
#' }
#' @source <https://gnomad.broadinstitute.org/news/2020-11-gnomad-v3-1-mitochondrial-dna-variants/>
"d_gnomAD_refined_long"
