# BSAvis Package
Please refer to [this repository](https://github.com/FadyMohareb/BSAvis_GP_2020) for a better understanding of the package.

```R
#Install BSAvis package
devtools::install_github("EG-lisy/BSAvis")
library(BSAvis)

#Inspect functions
#Common
?readBSA_vcf 
#BSAvis Package Functions
#Combined BSA and Plotting (wrapper functions)
?SNPindex_plot
?deltaSNPindex_plot  
?SNPratio_plot 
#Stepwise BSA and Plotting
?calc_SNPindex 
?calc_SNPratio 
?calc_deltaSNPindex 
?extract_chrIDs 
?filter_SNPindex 
?filter_SNPratio 
?plot_SNPindex 
?plot_SNPratio 
?plot_deltaSNPindex 
#BSAvis R-Shiny App
?BSAvis_shiny
#NOT to be run manually
?shiny_SNPindex 
?shiny_SNPratio 
?shiny_deltaSNPindex 
?slidingWindow
?shinyPlot_SNPindex 
?shinyPlot_deltaSNPindex 
?shinyPlot_SNPratio
```

# Authors

BSAvis (Version 1.0) was developed by:

_Elisabetta Galatola, Rebecca Guy, Weiyi Huang, Sweta Jajoriya, Claudia Rey-Carrascosa_

_Applied Bioinformatics MSc_<br>
_Cranfield University - Cranfield, Bedford, UK_<br>
_Academic Year: 2020-2021_

```
 MIT License
 Copyright (c) 2021 Mohareb's Lab

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
