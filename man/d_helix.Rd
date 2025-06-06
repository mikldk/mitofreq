% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-helix.R
\docType{data}
\name{d_helix}
\alias{d_helix}
\title{HelixMTdb raw data}
\format{
\subsection{\code{d_helix}}{

A tibble with 30 rows and 2 columns:
\describe{
\item{Position}{mtDNA position.}
\item{Ref}{Reference base.}
\item{Alt}{Variant base.}
\item{HGHom}{Variant counts in different TLHGs.}
\item{HGHomN}{Total from \code{HGHom}.}
\item{ExcludeReason}{Exclusion reason, \code{NA} if included in \code{d_helix_refined_long}.}
}
}
}
\source{
Bolze et al. (2020). "A catalog of homoplasmic and heteroplasmic mitochondrial DNA variants in humans" (
\url{https://www.biorxiv.org/content/10.1101/798264v3} / \url{https://doi.org/10.1101/798264})
}
\usage{
d_helix
}
\description{
Only information about homoplasmic variants.
}
\keyword{datasets}
