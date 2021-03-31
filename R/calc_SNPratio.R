#Calculate SNP-ratio

#' @title Calculate SNP-ratio
#' @description This function allows to calculate the SNP-ratio between two bulks (M/WT), as follows:
#' \deqn{(AD_ref.WT/(AD_ref.WT + AD_alt.WT)) - (AD_ref.M/(AD_ref.M + AD_alt.M))}
#' Note that the user can select specific variants to consider, by setting the "Variants" parameter to "SNP" (default) or "all" (respectively referring to SNPs or InDels+SNPs).
#'
#' @references Wachsman et al., 2017
#'
#' @param vcf.df vcf data frame
#' @param WTbulk wild-type bulk
#' @param Mbulk mutant bulk
#' @param Variants variants to be considered. Default is "SNP" (allowed: "SNP" or "all")
#' 
#' @return data frame (SNP-ratio)
#'
#' @export
#' @examples
#' vcf_df_SNPratio <- calc_SNPratio(vcf.df=vcf_df, 
#'                                  WTbulk="pool_S3781_minus", 
#'                                  Mbulk="pool_S3781_plus")

calc_SNPratio <- function(vcf.df, WTbulk, Mbulk, Variants="SNP") {
  
  #Create dataframe for each bulk AND include a new column with SNP-index
  vcf.df.WTbulk <- vcf.df %>% dplyr::filter(Indiv==WTbulk) 
  vcf.df.Mbulk <- vcf.df %>% dplyr::filter(Indiv==Mbulk) 
  
  #Stop the program and show message if user selects a not allowed 'Variants' value
  if (Variants!="SNP" & Variants!="all") {
    stop("The allowed values for the 'Variants' argument are: 'SNP' or 'all'. The latter will consider both InDels and SNPs.")
  }
  
  #Remove from the data frames rows corresponding to InDel variants (if Variants=="SNP")
  if (Variants=="SNP") {
    vcf.df.WTbulk <- vcf.df.WTbulk %>% dplyr::filter(nchar(GT_alleles)==3 &
                                                       grepl("[^*]{3}", GT_alleles))
    vcf.df.Mbulk <- vcf.df.Mbulk %>% dplyr::filter(nchar(GT_alleles)==3 &
                                                     grepl("[^*]{3}", GT_alleles))
  }
  
  #Join data frames of each bulk by same chromosome (ChromKey) and position (POS)
  vcf.df.jointBulks <- dplyr::inner_join(vcf.df.WTbulk, 
                                         vcf.df.Mbulk, 
                                         by = c("ChromKey","POS"), 
                                         copy = F, 
                                         suffix = c(".WT", ".M"))
  
  #Calculate SNP-ratio between the two bulks
  vcf.df.SNPratio <- vcf.df.jointBulks %>% dplyr::mutate("SNPratio" = ((as.numeric(AD_ref.WT)/(as.numeric(AD_ref.WT) + as.numeric(AD_alt.WT))) - (as.numeric(AD_ref.M)/(as.numeric(AD_ref.M) + as.numeric(AD_alt.M)))))
  
  return(vcf.df.SNPratio)
}