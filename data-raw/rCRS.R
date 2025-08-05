library(tidyverse)
library(readxl)
library(here)

##################################

rCRS_txt <- readLines(here("data-raw", "rCRS.fasta"))
rCRS_txt <- rCRS_txt[!grepl("^#", rCRS_txt)]
rCRS_txt <- strsplit(rCRS_txt, "", fixed = TRUE) |> unlist()
rCRS_txt
table(rCRS_txt)
rCRS_txt[3107]

d_rCRS <- tibble(Position = seq_along(rCRS_txt),
                 Ref = rCRS_txt)
d_rCRS

usethis::use_data(d_rCRS, overwrite = TRUE)
