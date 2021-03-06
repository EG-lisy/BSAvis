% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_SNPratio.R
\name{plot_SNPratio}
\alias{plot_SNPratio}
\title{Plot SNP-ratio}
\usage{
plot_SNPratio(
  vcf.df.SNPratio.filt,
  chrList,
  chrID,
  chr,
  min.SNPratio,
  degree = 2,
  span = 0.07,
  filename = paste0("plot_SNPratio_ch", chr),
  path = getwd(),
  dpi,
  width = 7.5,
  height = 5,
  units = "in"
)
}
\arguments{
\item{vcf.df.SNPratio.filt}{filtered data frame (containing both bulks)}

\item{chrList}{list of chromosome IDs}

\item{chrID}{chromosome ID of interest}

\item{chr}{chromosome number to print on plot}

\item{min.SNPratio}{min SNP ratio threshold}

\item{degree}{LOESS smoothing degree (default=2)}

\item{span}{LOESS smoothing span (default=0.07)}

\item{filename}{file name under which the file will be saved (default="plot_SNPratio_chX")}

\item{path}{path where the file will be saved (default=current working directory)}

\item{dpi}{resolution value. If no value is given, plots will be generated but not saved}

\item{width}{width value (default=7.5)}

\item{height}{height value (default=5)}

\item{units}{size units (default="in")}
}
\description{
This function allows plotting the SNP-ratio values across positions of a specified chromosome.
By setting the dpi parameter (resolution), the plot will be automatically saved in .TIF format.

\strong{Note:} \emph{for journal publications the preferred format is .TIF, and the minimum resolution is of 600 dpi or 1200 dpi. 
Always refer to journal-specific guidelines.}
}
\details{
The data frame returned by filter_SNPratio() is filtered by the input chromosome to restrict the data frame to the variants specific to the chosen chromosome. 
LOESS regression is then applied to the chromosome positions and SNP-ratio values, to smooth the resulting line.  
Degree and span parameters of the LOESS regression can be specified in the function arguments. 
If no values are specified, the default degree and span values will be applied. The smoothed SNP-ratio values are plotted against the chromosome position in a line plot. 

A cut-off line is included in the plot (equivalent to the minimum SNP-ratio required). If  the dpi argument is not specified, the plot will be shown in the plot panel; 
Conversely, by setting the dpi parameter, the graph will be saved in TIFF format. Additionally, the name with which to save the file, the directory, the height and width of the plot and their units can be customised as well. 
The plot will be saved with default values if different ones are not specified.
}
\examples{
## Use default values WITHOUT saving the plot
plot_SNPratio(vcf.df.SNPratio.filt=vcf_df_SNPratio_filt, 
              chrList=chromList, 
              chrID="SL4.0ch03", 
              chr=3, 
              min.SNPratio=0.3) 
## OR use default values AND save the plot
plot_SNPratio(vcf.df.SNPratio.filt=vcf_df_SNPratio_filt, 
              chrList=chromList, 
              chrID="SL4.0ch03", 
              chr=3, 
              min.SNPratio=0.3,
              dpi=1200) 
## OR customise default parameters
plot_SNPratio(vcf.df.SNPratio.filt=vcf_df_SNPratio_filt, 
              chrList=chromList, 
              chrID="SL4.0ch03", 
              chr=3, 
              min.SNPratio=0.3, 
              degree=2, 
              span=0.3, 
              filename="SNPratio_ch3", 
              path="Document/Plots", 
              dpi=1200, 
              width=20, 
              height=12, 
              units="cm")
}
\references{
Wachsman et al., 2017
}
