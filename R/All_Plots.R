#' All Plots data
#'
#' Data for the Rshiny application are stored here.
#' The data consists a list of plots for Alternate Binomial and Binomial Mixture distributions
#' modelled on the datasets from the R package "fitODBOD". Each element here consists seven
#' elements representing the seven datasets.
#'
#' @format A list with
#' \describe{
#' \item{\code{All_Data}}{All Datasets with binomial random variables and their frequencies}
#' \item{\code{ABD_Table}}{Table results from the Alternate Binomial distribution}
#' \item{\code{BMD_Table}}{Table results from the Binomial Mixture distributions}
#' \item{\code{Bin_Plot}}{Plot for the fitted Binomial distribution}
#' \item{\code{Add_Bin_Plot}}{Frequency plot for the fitted Additive Binomial distribution}
#' \item{\code{Beta_Corr_Bin_Freq_Plot}}{Frequency plot for the fitted Beta Correlated Binomial distribution}
#' \item{\code{Beta_Corr_Bin_Par_Plot}}{Parameter plot for the fitted Beta Correlated Binomial distribution}
#' \item{\code{COMP_Bin_Freq_Plot}}{Frequency plot for the fitted Composite Binomial distribution}
#' \item{\code{COMP_Bin_Par_Plot}}{Parameter plot for the fitted Composite Binomial distribution}
#' \item{\code{Corr_Bin_Freq_Plot}}{Frequency plot for the fitted Correlated Binomial distribution}
#' \item{\code{Corr_Bin_Par_Plot}}{Parameter plot for the fitted Correlated Binomial distribution}
#' \item{\code{Multi_Bin_Freq_Plot}}{Frequency plot for the fitted Multiplicative Binomial distribution}
#' \item{\code{Multi_Bin_Par_Plot}}{Parameter plot for the fitted Multiplicative Binomial distribution}
#' \item{\code{LMulti_Bin_Freq_Plot}}{Frequency plot for the fitted Lovinson Multiplicative Binomial distribution}
#' \item{\code{LMulti_Bin_Par_Plot}}{Parameter plot for the fitted Lovinson Multiplicative Binomial distribution}
#' \item{\code{Tri_Bin_Plot}}{Frequency plot for the fitted Triangular Binomial distribution}
#' \item{\code{Beta_Bin_Freq_Plot}}{Frequency plot for the fitted Beta Binomial distribution}
#' \item{\code{Beta_Bin_Par_Plot}}{Parameter plot for the fitted Beta Binomial distribution}
#' \item{\code{Kum_Bin_Freq_Plot}}{Frequency plot for the fitted Kumaraswamy Binomial distribution}
#' \item{\code{Kum_Bin_Par_Plot}}{Parameter plot for the fitted Kumaraswamy Binomial distribution}
#' \item{\code{Gam_Bin_Freq_Plot}}{Frequency plot for the fitted Gamma Binomial distribution}
#' \item{\code{Gam_Bin_Par_Plot}}{Parameter plot for the fitted Gamma Binomial distribution}
#' \item{\code{Grassia_Bin_Freq_Plot}}{Frequency plot for the fitted Grassia II Binomial distribution}
#' \item{\code{Grassia_Bin_Par_Plot}}{Parameter plot for the fitted Grassia II Binomial distribution}
#' \item{\code{GHGBeta_Bin_Freq_Plot}}{Frequency plot for the fitted Gaussian Hypergeometric Generalized Beta Binomial distribution}
#' \item{\code{GHGBeta_Bin_Par_Plot}}{Parameter plot for the fitted Gaussian Hypergeometric Generalized Beta Binomial distribution}
#' \item{\code{McGBB_Bin_Freq_Plot}}{Frequency plot for the fitted McDonald Generalized Beta Binomial distribution}
#' \item{\code{McGBB_Bin_Par_Plot}}{Parameter plot for the fitted McDonald Generalized Beta Binomial distribution}
#' }
#'
#' @examples
#' length(All_Plots$Bin_Plots) # No of plots in the list for all 7 datasets
#' length(All_Plots$All_Data) # 7 datasets
#'
"All_Plots"
