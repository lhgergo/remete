# gdrive_if_hobbit <- generate_interface_gdrive(server_id = "hobbit", result_dir = "remete_results", task_dir = "remete_tasks", tmp_dir = "~/tmp")
# save(gdrive_if_hobbit, file = "gdrive_if_hobbit.RData")


load("gdrive_if_hobbit.RData")
library(magrittr)
library(emhace.tools)

# rand_peptides <- sapply(1:100000, \(i) sample(rownames(protr::AABLOSUM62), 9) %>% paste0(collapse = ""))
# task_pkg <- PackIn(expr = RunNetMHCpan_ng(alleles = "HLA-A02:01", peptides = rand_peptides, threads = 10, version_number = "4.1",
#                                           software_path = "/home/lhgergo/Programok/netMHCpan-4.1/netMHCpan"),
#                    objects = "rand_peptides", libraries = c("emhace.tools", "purrr", "magrittr"), task_id = "prediction") |>
#   SendOut(interface = gdrive_if_hobbit)
# GetBack(task_pkg, interface = gdrive_if_hobbit)


##########
# going through the whole pipeline

ifobj_gdrive_test <- generate_interface_gdrive("test_server",
                                               task_dir = "remete_tasks_proba/",
                                               result_dir = "remete_results_proba/", file_dir = "remete_files_proba",
                                               tmp_dir = "~/tmp/")


rand_peptides <- sapply(1:100000, \(i) sample(rownames(protr::AABLOSUM62), 9) %>% paste0(collapse = ""))
task_pkg <- PackIn(expr = RunNetMHCpan_ng(alleles = "HLA-A02:01", peptides = rand_peptides, threads = 10, version_number = "4.1",
                                          software_path = "/home/lhgergo/Programok/netMHCpan-4.1/netMHCpan"),
                   objects = "rand_peptides", libraries = c("emhace.tools", "purrr", "magrittr"), task_id = "prediction") |>
  SendOut(interface = gdrive_if_hobbit)
GetBack(task_pkg, interface = gdrive_if_hobbit)
