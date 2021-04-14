#Shiny: SNP-indices

#' @title Shiny: Plot SNP-indices
#' @description Please note that this function is not to be run manually.
#' 
#' @param SNPindex.windows filtered data frame (containing both bulks)
#' @param chr chrosome ID
#' @param wtAlpha wild-type graph (default=1)
#' @param mAlpha mutant graph (default=1)
#' @param ranges axes ranges (x,y)
#' 
#' @export shinyPlot_SNPindex

shinyPlot_SNPindex <- function(vcf.df.window.SNPindex, chr, wtAlpha=1, mAlpha=1, ranges) {
  
  #This function includes in the arguments 'wtAlpha' and 'mAlpha' (referring to
  #the alpha argumet in geom_line()) in order to be able to set lines as 'invisible' 
  #if that is desired by the user while using the shiny App.
  #The argument 'ranges' will allow to zoom in the plots while using the shiny App. 
  
  #Plot SNP-indexes of both pools. 
  ggplot2::ggplot() +
    ggplot2::geom_line(data=vcf.df.window.SNPindex, aes(mid, mean_SNPindex.WT, color="WT pool"), size=0.75, alpha=wtAlpha) +  
    ggplot2::geom_line(data=vcf.df.window.SNPindex, aes(mid, mean_SNPindex.M, color="Mutant pool"), size=0.75, alpha=mAlpha) +
    ggplot2::geom_hline(yintercept = 0.5, lty=2) + 
    ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
    ggplot2::scale_x_continuous(labels=function(x)x/1000000) +
    ggplot2::ylim(0, 1) + 
    ggplot2::xlab("Position (Mb)") + 
    ggplot2::ylab("SNP-index") +
    ggplot2::labs(title = paste("Chromosome", chr)) + 
    ggplot2::theme(legend.title = element_blank())
}
