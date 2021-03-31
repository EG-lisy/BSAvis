#Read file and calculate SNP Index

#' @title Calculate (SNP index)
#' @description This function is used to sparate (filter) the two bulks, and calculate their (SNP index) as follows:
#' \deqn{SNPindex=AD_alt/(AD_ref + AD_alt)} 
#' Bulks get joined together in a single data frame.
#' 
#' Note that the user can select specific variants to consider, 
#' by setting the "Variants" parameter to "SNP" (default) or "all" (InDels+SNPs).
#'
#' @param vcf.df Data frame of the vcf file
#' @param WTbulk Wild-Type pool
#' @param Mbulk Mutant pool 
#' @param Variants variants to be considered. Default is "SNP" (allowed: "SNP" or "all")
#'
#' @return A dataframe with the joint bulks
#'
#' @export
#' @examples
#' \strong{Calculate SNP-index of both bulks (by default, only SNPs are considered)}
#' vcf_df_SNPindex <- calc_SNPindex(vcf.df=vcf_df, 
#'                                WTbulk="pool_S3781_minus", 
#'                                Mbulk="pool_S3781_plus")
#' \strong{Calculate SNP-index considering InDels and SNPs}
#' vcf_df_SNPindex <- calc_SNPindex(vcf.df=vcf_df, 
#                                  WTbulk="pool_S3781_minus", 
#                                  Mbulk="pool_S3781_plus",
#                                  Variants="all")

calc_SNPindex <- function(vcf.df, WTbulk, Mbulk, Variants="SNP") {
  
  #Create dataframe for each bulk AND include a new column with SNP index
  vcf.df.WTbulk <- vcf.df %>% dplyr::filter(Indiv==WTbulk) %>% dplyr::mutate("SNPindex"= as.numeric(AD_alt)/(as.numeric(AD_ref) + as.numeric(AD_alt)))
  vcf.df.Mbulk <- vcf.df %>% dplyr::filter(Indiv==Mbulk) %>% dplyr::mutate("SNPindex"= as.numeric(AD_alt)/(as.numeric(AD_ref) + as.numeric(AD_alt)))
  
  #Stop the program and show message if user selects a non-allowed 'Variants' value
  if (Variants!="SNP" & Variants!="all") {
    stop("The allowed values for the 'Variants' argument are: 'SNP' or 'all'. The latter will consider both InDels and SNPs.")
  }
  
  #Remove from dataframes those rows corresponding to INDEL variants (if Variants=="SNP")
  if (Variants=="SNP") {
    vcf.df.WTbulk <- vcf.df.WTbulk %>% dplyr::filter(nchar(GT_alleles)==3 &
                                                       grepl("[^*]{3}", GT_alleles))
    vcf.df.Mbulk <- vcf.df.Mbulk %>% dplyr::filter(nchar(GT_alleles)==3 &
                                                     grepl("[^*]{3}", GT_alleles))
  }
  
  #Join dataframes of each bulk by same chromosome (ChromKey) and position
  vcf.df.jointBulks <- dplyr::inner_join(vcf.df.WTbulk, 
                                         vcf.df.Mbulk, 
                                         by = c("ChromKey","POS"), 
                                         copy = F, 
                                         suffix = c(".WT", ".M"))
  
  return(vcf.df.jointBulks)
}
