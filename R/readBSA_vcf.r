#Read vcf file

#' @title Read vcf file
#' @description This function allows to read a vcf file, converting it inot a data frame. 
#' Format fields get separated as follows: AD, DP, GQ, GT. 
#' 
#' Allelic depths (AD) get split into reference and alterate AD values ("AD_ref" and "AD_alt").
#' 
#' @param file vcf file
#'
#' @return a list containing meta information
#'
#' @export
#' @example
#' vcf_list <- readBSA_vcf(file="dataset1.vcf")

readBSA_vcf <- function(file) {
  
  vcf <- vcfR::read.vcfR(file, verbose = FALSE)

  vcf.df <- vcfR::vcfR2tidy(vcf,
                            format_fields = c("AD", "DP", "GQ", "GT"),  
                            gt_column_prepend = "")
  
  #Limit dataframe to columns of interest
  vcf.df <- vcf.df$gt
  
  #Split AD column into reference and alterate AD values
  vcf.df <- vcf.df %>% tidyr::separate(AD, sep = ",", into = c("AD_ref", "AD_alt"))
  
  vcf.list <- list(meta=vcf@meta, df=vcf.df)
  
  return(vcf.list)
}
