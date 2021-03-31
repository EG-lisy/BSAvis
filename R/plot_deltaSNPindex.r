#Plot delta(SNP Index)
#' @title Plot delta(SNP index)
#' @description This function allows plotting delta(SNP Index).
#' Whenever the DPI parameter gets specified, the plot will be saved in .TIF format with the specified resolution (dpi value).
#' 
#' #' \strong{Note:} \emph{for journal publications the preferred format is .TIF, and the minimum resolution is of 600 dpi or 1200 dpi. 
#' Always refer to journal-specific guidelines.}
#'
#' @param vcf.df.window.delta filtered data frame (containing both bulks)
#' @param Chrom chromosome ID
#' @param DPI resolution value. If no value is given, plots will be generated but not saved. 
#' @param Width width value. Default=7.5
#' @param Height height value. Default=5
#' @param Units size units. Default="in" (inches)
#'
#' @export
#' @examples
#' \strong{Plot SNP-index}
#' plot_deltaSNPindex(vcf.df.bulks=vcf_df_window_delta,
#'                    Chrom=3)
#' \strong{Save plot (specified resolution - tiff format)}
#' plot_deltaSNPindex(vcf.df.bulks=vcf_df_window_delta,
#'                    Chrom=3,
#'                    DPI=1200)
#' \strong{Save plot with specified width and height. The default unit is set to inches (Units="in")}
#' plot_deltaSNPindex(vcf.df.bulks=vcf_df_window_delta,
#'                    Chrom=3,
#'                    DPI=1200,
#'                    Height=9,
# '                   Width=6)


plot_deltaSNPindex <- function(vcf.df.window.delta, Chrom, DPI, Width=7.5, Height=5, Units="in"){
  
  if(missing(DPI)) {
    
    #Print message to let user know that the plot was not saved and show required arguments to save it
    message("Delta(SNP-index) plot is being displayed. In order to save it, please specify DPI (and Height and Width, if different values from the default ones are desired).")
    
    #Plot delta SNP index
    ggplot2::ggplot() +
      ggplot2::geom_line(data=vcf.df.window.delta, aes(mid, delta_SNPindex), size=0.75) +  
      ggplot2::geom_hline(yintercept = 0, lty=2) + 
      ggplot2::coord_cartesian(expand=FALSE) +
      ggplot2::scale_x_continuous(labels=function(x)x/1000000) +
      ggplot2::ylim(-1, 1) + 
      ggplot2::xlab("Position (Mb)") + 
      ggplot2::ylab("delta(SNP-index)") +
      ggplot2::labs(title = paste("Chromosome", Chrom)) 
  }
  
  else {
    ggsave(paste0("DeltaSNPindex_ch", Chrom, ".tiff"), device = "tiff", dpi = DPI, width = Width, height = Height, units = Units) 
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