# LOADING THINGS ----------
setwd("../remete/")
library(purrr)
library(magrittr)
library(emhace.tools)
library(remete)

set.seed(1234)
alleles_unq <- readLines("../SARS-CoV-2-blindspot/data/alleles.txt")
eps <- map_chr(1:20000, ~protr::AABLOSUM62 %>% rownames %>% sample(size = 9, replace = TRUE) %>% paste0(collapse = ""))
# eps <- c(eps, "ALALLALAL*")

# PERFORMING PREDICTIONS REMOTELY ----------
load("gdrive_if_brcws.RData")
taskobj <- PackIn(expr = emhace.tools::RunNetMHCpan(alleles = alleles_unq[1:4], peptides = eps,
                                                    paired_input = FALSE, output_format = "wide",
                                                    threads = 4,
                                                    result_files_location = "/home/lhgergo/resfiles/",
                                                    software_path = "/home/lhgergo/Programok/netMHCpan-4.1/netMHCpan"),
                  objects = c("alleles_unq", "eps"), libraries = "emhace.tools", task_id = "test_prediction1000") %>%
  SendOut(interface = "gdrive_if_brcws")


recogn_matrices <- GetBack(c(taskobj, interface = "gdrive_if_brcws"))

PackIn(3+4, task_id = "minortask") %>% SendOut(interface = "gdrive_if_brcws")

taskobj <- c(task_id = "2023-02-24-22-51-42-minortask", task_package_path = "/home/lhgergo/tmp//2023-02-24-22-51-42-minortask", interface = "gdrive_if_brcws")
GetBack(c(taskobj, interface = "gdrive_if_brcws"))
