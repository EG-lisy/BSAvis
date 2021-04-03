#Plot (SNP Index)

#' @title Plot (SNP index)
#' @description This function allows plotting the (SNP Index).
#' Whenever the DPI parameter gets specified, the plot will be saved in .TIF format with the specified resolution (dpi value).
#' 
#' \strong{Note:} \emph{for journal publications the preferred format is .TIF, and the minimum resolution is of 600 dpi or 1200 dpi. 
#' Always refer to journal-specific guidelines.}
#' 
#' @param vcf.df.window.SNPindex filtered data frame (containing both bulks)
#' @param Chrom chromosome ID
#' @param DPI resolution value. If no value is given, plots will be generated but not saved
#' @param Width width value (default=7.5)
#' @param Height height value (default=5)
#' @param Units size units (default="in")
#' 
#' @export
#' @examples
#' \strong{Plot SNP-index of both bulks (wild-type and mutant)}
#' plot_SNPindex(vcf.df.window.SNPindex=vcf_df_window,
#'               Chrom=3)
#' \strong{Save plot (specified resolution - tiff format)}
#' plot_SNPindex(vcf.df.bulks=vcf_df_window,
#'               Chrom=3,
#'               DPI=1200)
#' \strong{Save plot with specified width and height. The default unit is set to inches (Units="in")}
#' plot_SNPindex(vcf.df.bulks=vcf_df_window,
#'               Chrom=3,
#'               DPI=1200,
#'               Height=9,
#'               Width=6)

plot_SNPindex <- function(vcf.df.window.SNPindex, Chrom, DPI, Width=7.5, Height=5, Units="in") {
  
  if(missing(DPI)){
    
    #Print message to let user know that the plot was not saved and show required arguments to save it
    message("SNP-index plot is being displayed. In order to save it, please specify DPI (and Height and Width if different values from the default ones are desired).")
    
    #Plot SNP indexes of both pools
    ggplot2::ggplot() +
      ggplot2::geom_line(data=vcf.df.window.SNPindex, aes(mid, mean_SNPindex.WT, color="WT pool"), size=0.75) +  
      ggplot2::geom_line(data=vcf.df.window.SNPindex, aes(mid, mean_SNPindex.M, color="Mutant pool"), size=0.75) +
      ggplot2::geom_hline(yintercept = 0.5, lty=2) + 
      ggplot2::coord_cartesian(expand=FALSE) +
      ggplot2::scale_x_continuous(labels=function(x)x/1000000) +
      ggplot2::ylim(0, 1) + 
      ggplot2::xlab("Position (Mb)") + 
      ggplot2::ylab("SNP-index") +
      ggplot2::labs(title = paste("Chromosome", Chrom)) + 
      ggplot2::theme(legend.title = element_blank())
  }
  
  else {
    ggsave(paste0("SNPindex_ch", Chrom, ".tiff"), device = "tiff", dpi = DPI, width = Width, height = Height, units = Units) 
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