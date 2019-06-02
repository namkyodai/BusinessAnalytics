#this package only work on 64 version of R. pls change the global setting before running the code.
#https://www.r-bloggers.com/automate-your-kpi-forecasts-with-only-1-line-of-r-code-using-autots/
library(RemixAutoML)
library(data.table)
library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)
library(magick)
library(grid)
# IMPORT DATA FROM REMIX INSTITUTE BOX ACCOUNT ----------

walmart_store_sales_data = data.table::fread("<a class="vglnk" href="https://remixinstitute.box.com/shared/static/9kzyttje3kd7l41y1e14to0akwl9vuje.csv" rel="nofollow"><span>https</span><span>://</span><span>remixinstitute</span><span>.</span><span>box</span><span>.</span><span>com</span><span>/</span><span>shared</span><span>/</span><span>static</span><span>/</span><span>9kzyttje3kd7l41y1e14to0akwl9vuje</span><span>.</span><span>csv</span></a>", header = T, stringsAsFactors = FALSE)


