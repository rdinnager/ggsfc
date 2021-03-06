#' Simulated SNP frequency data
#'
#' Dataset with Single Nucleotide Polymorphism (SNPs) frequencies for three populations simulated using the package \code{slimr}
#'
#' @format A data frame with 4929 rows and 6 variables:
#' \describe{
#'   \item{\code{freq}}{double. Frequency of the alternative allele (e.g. a novel mutation).}
#'   \item{\code{index}}{double. A unique id by row.}
#'   \item{\code{pop}}{integer. Population number of the SNP.}
#'   \item{\code{type}}{character. Type of mutation. These are all "m1". This column can be ignored.}
#'   \item{\code{pos}}{integer. Position on the genome of the SNP.}
#'   \item{\code{sel}}{double. Selection coefficient for the simulated mutation.}
#' }
"sim_snps"