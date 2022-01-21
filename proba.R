# #################### TESTING -----------
# running server
remete_server_session()

# simple task
a = sample(1:100, size = 40)
b = sample(1:100, size = 40)

out <- send_to_remote({rcorr(a, b, type = "spearman")}, objs = c("a", "b"), libs = c("Hmisc"), wait_for_result = TRUE)
out2 <- send_to_remote({rcorr(a, b, type = "spearman")}, objs = c("a", "b"), libs = c("Hmisc"), wait_for_result = FALSE)

runningdf <- send_to_remote("show_ongoing_tasks")
load_result("2umaacdooj")

# a whole script

send_to_remote({
  a <- c(1:10)
  b <- c(91:100)
  Hmiss::rcorr(a, b)
}, wait_for_result = TRUE)

send_to_remote({
  install.packages("abglassow")
}, wait_for_result = TRUE)



# ############### trying out a netmhcpan run
list.files("~/Dokumentumok/LABOR/SARS-CoV-2-immunoadaptation/netMHCpan-tools_functions/", full.names = TRUE) %>%
  extract(!grepl("db_playground", .)) %>% lapply(source)
#
# list.files("~/Dokumentumok/LABOR/SARS-CoV-2-immunoadaptation/netMHCpan-tools_functions/", full.names = TRUE) %>%
#   extract(!grepl("db_playground", .)) %>% lapply(function(flnm) {
#     x <- readLines(flnm)
#     x[substr(x, 1, 7) == "library"] %>% gsub("library\\(", "", .) %>% gsub("\\)", "", .)
#   }) %>% unlist()
#
# fnctns <- lsf.str() %>% as.vector()
# peptides <- purrr::map_chr(1:1000, ~sample(rownames(protr::AABLOSUM100), 9, replace = TRUE) %>% paste0(collapse = ""))
# alleles <- readLines("../SARS-CoV-2-immunoadaptation_data/alleles.txt")[1:32]
#
# recogn_matrix_rp <- send_to_remote(x = RunNetMHCpan4.0(alleles = alleles,
#                                                        peptides = peptides,
#                                                        type = "rp",
#                                                        output_format = "wide",
#                                                        threads = 32),
#                objs = c(fnctns, "amino_acids", "peptides", "alleles", "configs"), libs = c("magrittr", "reshape2", "furrr", "parallel"))
#
