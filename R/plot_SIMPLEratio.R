#Plot SIMPLE-ratio

#' @title Plot SIMPLE-ratio
#' @description This function allows plotting the SIMPLE-ratio (Wachsman et al., 2017).\n
#' The SIMPLE-ratio gets filtered by the minimum SIMPLE-ratio value (set by the user), before performing LOESS smoothing.\n
#' The plot gets generated based on the chromosome location of each SNP against the LOESS-fitted SIMPLE-ratio.\n
#' Whenever the DPI parameter gets specified, the plot will be saved in .TIF format with the specified resolution (dpi value).\n\n
#' Note: for journal publications the preferred format is .TIF, and the minimum resolution is of 600 dpi or 1200 dpi. Always refer to journal-specific guidelines.
#' 
#' @param vcf.df.SIMPLEratio SIMPLE-ratio data frame (containing both bulks)
#' @param min_SIMPLEratio minimum SIMPLE-ratio filtering value considered
#' @param Chrom chromosome ID
#' @param DPI resolution value. If no value is given, plots will be generated but not saved. 
#' @export
#' 
#' @examples
#' Plot SIMPLE-ratio for a specific chromosome\n
#' plot_SIMPLEratio(vcf.df.SIMPLEratio=vcf_df_SIMPLEratio, 
#' min_SIMPLEratio=0.1, 
#' Chrom=7)\n
#' Save plot (specified resolution - tiff format)\n
#' plot_SIMPLEratio(vcf.df.SIMPLEratio=vcf_df_SIMPLEratio, 
#' min_SIMPLEratio=0.1, 
#' Chrom=7,
#' DPI=1200)

 
plot_SIMPLEratio <- function(vcf.df.SIMPLEratio, min_SIMPLEratio, Chrom, DPI) {

#Filter by min SIMPLE-ratio
vcf.df.SIMPLEratio <- vcf.df.SIMPLEratio %>% dplyr::filter(SIMPLEratio >= min_SIMPLEratio) 

#Filter by given chromosome
vcf.df.SIMPLEratio.ch <- vcf.df.SIMPLEratio %>% dplyr::filter(ChromKey==(Chrom + 1))

#Apply LOESS smoothing to SIMPLE-ratio
loess.fitted <- stats::loess(SIMPLEratio ~ POS, degree=2, span=0.3, data=vcf.df.SIMPLEratio.ch)

#Plot the chromosomal location of each SNP against the LOESS-fitted SIMPLE-ratio
if(missing(DPI)) {
  vcf.df.SIMPLEratio.ch %>% dplyr::mutate(smooth = loess.fitted$fitted) %>%
    ggplot2::ggplot(aes(POS, smooth)) +
    ggplot2::geom_point(size = 0.3) +
    ggplot2::geom_hline(yintercept=min_SIMPLEratio, lty=2) +
    ggplot2::coord_cartesian(expand=FALSE) +
    ggplot2::scale_x_continuous(labels=function(x)x/1000000) +
    ggplot2::ylim(0, 1) +
    ggplot2::xlab("Position (Mb)") + 
    ggplot2::ylab("SIMPLE-ratio") +
    ggplot2::labs(title = paste("Chromosome", Chrom)) 
}

else {
  ggsave(paste0("SIMPLEratio_ch", Chrom, ".tiff"), device = "tiff", dpi = DPI) 
}
}
