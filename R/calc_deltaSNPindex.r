#Calculate delta(SNP Index)

#' @title Calculate delta(SNP index)
#' @description This function is used to calculate the delta(SNP index) using the calculated mean of the mutant (M) and wild-type (WT) indices, as follows:
#' \deqn{mean_M_SNPindex - mean_WT_SNPindex.WT}
#'
#' @param vcf.df.window data frame obtained after running the slidingWindow() function
#'
#' @return data frame containing the delta(SNP index) values
#'
#' @export
#' @example
#' vcf_df_window_delta <- calc_deltaSNPindex(vcf.df.window=vcf_df_window)


calc_deltaSNPindex <- function(vcf.df.window) {
  
  #Calculate delta SNP-index and add it as new column
  vcf.df.deltaSNPindex <- vcf.df.window %>% dplyr::mutate("delta_SNPindex" = (mean_SNPindex.M - mean_SNPindex.WT))
  return(vcf.df.deltaSNPindex)
}