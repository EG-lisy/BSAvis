#Plot SNP-ratio

#' @title Plot SNP-ratio
#' @description This function allows plotting the SNP-ratio (Wachsman et al., 2017).
#' The SNP-ratio gets filtered by the minimum SNP-ratio value (set by the user), before performing LOESS smoothing.
#' The plot gets generated based on the chromosome location of each SNP against the LOESS-fitted SNP-ratio.
#' Whenever the DPI parameter gets specified, the plot will be saved in .TIF format with the specified resolution (dpi value).
#' \strong{Note:} \emph{for journal publications the preferred format is .TIF, and the minimum resolution is of 600 dpi or 1200 dpi. 
#' Always refer to journal-specific guidelines.}
#' 
#' @references Wachsman et al., 2017
#' 
#' @param vcf.df.SNPratio SNP-ratio data frame (containing both bulks)
#' @param min_SNPratio minimum SNP-ratio value to consider (default=0.1)
#' @param Degree LOESS smoothing degree (default=2)
#' @param Span LOESS smoothing span (default=0.3)
#' @param Chrom chromosome ID
#' @param DPI resolution value. If no value is given, plots will be generated but not saved
#' @param Width width value (default=7.5)
#' @param Height height value (default=5)
#' @param Units size units (default="in")
#' 
#' @export
#' 
#' @examples
#' \strong{Plot SNP-ratio for a specific chromosome}
#' plot_SNPratio(vcf.df.SNPratio=vcf_df_SNPratio, 
#'               min_SNPratio=0.1, 
#'               Chrom=7)
#' \strong{Save plot (specified resolution - tiff format)}
#' plot_SNPratio(vcf.df.SNPratio=vcf_df_SNPratio, 
#'               min_SNPratio=0.1, 
#'               Chrom=7,
#'               DPI=1200)
#' \strong{Save plot with specified width and height. The default unit is set to inches (Units="in")}
#' plot_SNPratio(vcf.df.SNPratio=vcf_df_SNPratio, 
#'               min_SNPratio=0.1, 
#'               Chrom=7,
#'               DPI=1200)
#'               Height=9,
#'               Width=6)

 
plot_SNPratio <- function(vcf.df.SNPratio, min_SNPratio=0.1, Degree=2, Span=0.3, Chrom, DPI, Width=7.5, Height=5, Units="in") {
  
  #If it is the case, print messages to let user know that default values are being used 
  if (min_SNPratio==0.1) {
    message("=> Filtering variants with DEFAULT minimum SNP ratio value (min_SIMPLEratio = 0.1)...")
  }
  
  if (Degree==2 & Span==0.3) {
    message("=> Applying LOESS smoothing with DEFAULT degree and span values (Degree = 2, Span = 0.3)...")
  }
  else if (Degree==2) {
    message("=> Applying LOESS smoothing with DEFAULT degree value (Degree = 2)...")
  }
  else if (Span==0.3) {
    message("=> Applying LOESS smoothing with DEFAULT span value (Span = 0.3)...")
  }
  
  #Filter by min SNP-ratio
  vcf.df.SNPratio <- vcf.df.SNPratio %>% dplyr::filter(SNPratio >= min_SNPratio) 
  
  #Filter by given chromosome
  vcf.df.SNPratio.ch <- vcf.df.SNPratio %>% dplyr::filter(ChromKey==(Chrom + 1))
  
  #Apply LOESS smoothing to SNP-ratio
  loess.fitted <- stats::loess(SNPratio ~ POS, degree=Degree, span=Span, data=vcf.df.SNPratio.ch)
  
  #Plot the chromosomal location of each SNP against the LOESS-fitted SIMPLE-ratio
  if(missing(DPI)) {
    
    #Print message to let user know that the plot was not saved and show required arguments to save it
    message("SNP-ratio plot is being displayed. In order to save it, please specify DPI (and Height and Width, if different values from the default ones are desired).")
    
    vcf.df.SNPratio.ch %>% dplyr::mutate(smooth = loess.fitted$fitted) %>%
      ggplot2::ggplot(aes(POS, smooth)) +
      ggplot2::geom_point(size = 0.3) +
      ggplot2::geom_hline(yintercept=min_SNPratio, lty=2) +
      ggplot2::coord_cartesian(expand=FALSE) +
      ggplot2::scale_x_continuous(labels=function(x)x/1000000) +
      ggplot2::ylim(0, 1) +
      ggplot2::xlab("Position (Mb)") + 
      ggplot2::ylab("SNP-ratio") +
      ggplot2::labs(title = paste("Chromosome", Chrom)) 
  }
  
  else {
    ggsave(paste0("SNPratio_ch", Chrom, ".tiff"), device = "tiff", dpi = DPI, width = Width, height = Height, units = Units)
    if (Width==7.5 & Height==5) {
      message("Plot was saved with DEFAULT width and height values (Width = 7.5 inches, Height = 5 inches).") 
    }
    else if (Width==7.5) {
      message("Plot was saved with DEFAULT width value (Width = 7.5 inches).")  
    }
    else if (Height==5) {
      message("Plot was saved with DEFAULT height value (Height = 5 inches).")  
    }
  }
}