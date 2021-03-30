#Plot (SNP Index)

#' @title Plot (SNP index)
#' @description This function allows plotting the (SNP Index).
#' Whenever the DPI parameter gets specified, the plot will be saved in .TIF format with the specified resolution (dpi value).
#' Note: for journal publications the preferred format is .TIF, and the minimum resolution is of 600 dpi or 1200 dpi. Always refer to journal-specific guidelines.
#' 
#' @param vcf.df.bulks.filt filtered data frame (containing both bulks)
#' @param Chrom chromosome ID
#' @param DPI resolution value. If no value is given, plots will be generated but not saved. 
#' @export
#' @examples
#' Plot SNP-index of both bulks (wild-type and mutant)
#' plot_SNPindex(vcf.df.bulks=vcf_df_window,
#' Chrom=3)
#' Save plot (specified resolution - tiff format)
#' plot_SNPindex(vcf.df.bulks=vcf_df_window,
#' Chrom=3,
#' DPI=1200)


plot_SNPindex <- function(poolsVCF_df, Chrom, DPI) {
  
  if(missing(DPI)){
    #Plot SNP indexes of both pools
    ggplot2::ggplot() +
      ggplot2::geom_line(data=vcf.df.bulks.filt, aes(mid, mean_SNPindex.WT, color="WT pool"), size=0.75) +  
      ggplot2::geom_line(data=vcf.df.bulks.filt, aes(mid, mean_SNPindex.M, color="Mutant pool"), size=0.75) +
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
    ggsave(paste0("SNPindex_ch", Chrom, ".tiff"), device = "tiff", dpi = DPI)
  }
}