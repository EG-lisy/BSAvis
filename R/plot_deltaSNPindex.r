#Plot delta(SNP Index)
#' @title Plot delta(SNP index)
#' @description This function allows plotting delta(SNP Index).\n 
#' Whenever the DPI parameter gets specified, the plot will be saved in .TIF format with the specified resolution (dpi value).\n\n
#' Note: for journal publications the preferred format is .TIF, and the minimum resolution is of 600 dpi or 1200 dpi. Always refer to journal-specific guidelines.
#' 
#'
#' @param vcf.df.bulks.filt filtered data frame (containing both bulks)
#' @param Chrom chromosome ID
#' @param DPI resolution value. If no value is given, plots will be generated but not saved. 
#'
#'
#' @export
#' @examples
#' Plot SNP-index\n
#' plot_deltaSNPindex(vcf.df.bulks=vcf_df_window_delta,
#' Chrom=3)\n
#' Save plot (specified resolution - tiff format)\n
#' plot_deltaSNPindex(vcf.df.bulks=vcf_df_window_delta,
#' Chrom=3,
#' DPI=1200)


plot_deltaSNPindex <- function(poolsVCF_df, Chrom, DPI){
  
  if(missing(DPI)){
    #Plot delta SNP index
    ggplot2::ggplot() +
      ggplot2::geom_line(data=vcf.df.bulks.filt, aes(mid, delta_SNPindex), size=0.75) +  
      ggplot2::geom_hline(yintercept = 0, lty=2) + 
      ggplot2::coord_cartesian(expand=FALSE) +
      ggplot2::scale_x_continuous(labels=function(x)x/1000000) +
      ggplot2::ylim(-1, 1) + 
      ggplot2::xlab("Position (Mb)") + 
      ggplot2::ylab("delta(SNP-index)") +
      ggplot2::labs(title = paste("Chromosome", Chrom)) 
  }
  
  else {
    ggsave(paste0("DeltaSNPindex_ch", Chrom, ".tiff"),device = "tiff" ,dpi = DPI) 
  }
}