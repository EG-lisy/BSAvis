#Calculate SIMPLE-ratio

#' @title Calculate SIMPLE-ratio
#' @description First of all, this function creates a data frame for each bulk, which includes a column containing the SNP-index values. 
#' Then, rows containing InDel variants (i.e. insertions/deletions) get removed, and the data frames get joined together by chromosome (ChromKey) and position (POS).\n
#' Finally, the SIMPLE-ratio between the two bulks (M/WT) gets calculated as follows:
#' \deqn{(AD_ref.WT/(AD_ref.WT + AD_alt.WT)) - (AD_ref.M/(AD_ref.M + AD_alt.M))}\n
#' Reference: Wachsman et al., 2017
#'
#' @param vcf.df vcf data frame
#' @param WTbulk wild-type bulk
#' @param Mbulk mutant bulk
#' 
#' @return data frame (SIMPLE-ratio)
#'
#' @export
#' @examples
#' vcf_df_SIMPLEratio <- calc_SIMPLEratio(vcf.df=vcf_df, 
#' WTbulk="pool_S3781_minus", 
#' Mbulk="pool_S3781_plus")

calc_SIMPLEratio <- function(vcf.df, WTbulk, Mbulk) {
  
  #Create dataframe for each bulk sample AND include a new column with SNP-index
  vcf.df.WTbulk <- vcf.df %>% dplyr::filter(Indiv==WTbulk) 
  vcf.df.Mbulk <- vcf.df %>% dplyr::filter(Indiv==Mbulk) 
  
  #Remove rows ine ach dataframe corresponding to INDEL variants 
  vcf.df.WTbulk <- vcf.df.WTbulk %>% dplyr::filter(nchar(GT_alleles)==3 &
                                                     grepl("[^*]{3}", GT_alleles))
  vcf.df.Mbulk <- vcf.df.Mbulk %>% dplyr::filter(nchar(GT_alleles)==3 &
                                                   grepl("[^*]{3}", GT_alleles))
  
  #Join dataframes of each bulk by same chromosome (ChromKey) and position
  vcf.df.jointBulks <- dplyr::inner_join(vcf.df.WTbulk, 
                                         vcf.df.Mbulk, 
                                         by = c("ChromKey","POS"), 
                                         copy = F, 
                                         suffix = c(".WT", ".M"))
  
  #Calculate SIMPLE-ratio between the two bulks
  vcf.df.SIMPLE_ratio <- vcf.df.jointBulks %>% dplyr::mutate("SIMPLEratio" = ((as.numeric(AD_ref.WT)/(as.numeric(AD_ref.WT) + as.numeric(AD_alt.WT))) - (as.numeric(AD_ref.M)/(as.numeric(AD_ref.M) + as.numeric(AD_alt.M)))))
  
  return(vcf.df.SIMPLE_ratio)
}
