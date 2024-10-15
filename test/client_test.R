library(magrittr)
library(purrr)
# # In this file I aim to test the client side of the package

# # testing the "file" interface ----------
# ifobj_file <- generate_interface_file(server_id = "proba_file", # a unique identifier for the server to be used (e.g. the name of the remote computer)
#                                       task_dir =  "/home/lhgergo/tasks", # the name of the directory where task packages will be stored for transmission
#                                       result_dir = "/home/lhgergo/results",
#                                       tmp_dir = "/home/lhgergo/tmp")  # a local temporary directory
# save(ifobj_file, file = "test/ifobj_file.RData")
load("test/ifobj_file.RData")

taskobj <- PackIn(sample(1:100, 50)) %>% SendOut(ifobj_file)
GetBack(taskobj, interface = ifobj_file, simplified_output = T)

# # testing the "gdrive" interface ----------
# ifobj_gdrive <- generate_interface_gdrive(server_id = "proba_gdrive", # a unique identifier for the server to be used (e.g. the name of the remote computer)
#                                       task_dir =  "tasks_test", # the name of the directory where task packages will be stored during transmission (on gdrive)
#                                       result_dir = "results_test", # the name of the directory where result packages will be stored during transmission (on gdrive)
#                                       file_dir = "files_test", # the name of the directory where files will be stored during transmission (on gdrive)
#                                       tmp_dir = "/home/lhgergo/tmp")  # a local temporary directory
# save(ifobj_gdrive, file = "test/ifobj_gdrive.RData")

load("test/ifobj_gdrive.RData")
peps_unq <- sapply(1:100000, \(x) sample(rownames(protr::AABLOSUM62), 9) %>% paste0(collapse = ""))
taskobj <- emhace.tools::RunNetMHCpan_ng(alleles = "HLA-A02:01", peptides = peps_unq, version_number = "4.1",
                                         software_path = "/home/lhgergo/Programok/netMHCpan-4.1/netMHCpan") |>
  PackIn(objects = "peps_unq", libraries = c("purrr", "emhace.tools", "magrittr")) %>%
  SendOut(ifobj_gdrive)

peps_unq <- sapply(1:100000, \(x) sample(rownames(protr::AABLOSUM62), 9) %>% paste0(collapse = ""))
taskobj2 <- emhace.tools::RunNetMHCpan_ng(alleles = "HLA-A02:01", peptides = peps_unq, version_number = "4.1",
                                         software_path = "/home/lhgergo/Programok/netMHCpan-4.1/netMHCpan") |>
  PackIn(objects = "peps_unq", libraries = c("purrr", "emhace.tools", "magrittr")) %>%
  SendOut(ifobj_gdrive)

GetBack(taskobj, interface = ifobj_gdrive, simplified_output = T)
GetBack(taskobj2, interface = ifobj_gdrive, simplified_output = T)

# experimenting with async, managing processes from remote -----------
GetBack(taskobj, interface = ifobj_gdrive, simplified_output = F)

process_query <- PackIn(system("ps aux | grep /usr/bin/R", intern = T)) |> SendOut(ifobj_gdrive)
process_query_R <- PackIn(system("ps aux | grep /usr/bin/R", intern = T)) |> SendOut(ifobj_gdrive)
process_query_nnalign <- PackIn(system("ps aux | grep nnalign", intern = T)) |> SendOut(ifobj_gdrive)
process_kill_nnalign <- PackIn(system("kill 12231", intern = T)) |> SendOut(ifobj_gdrive)

GetBack(process_query_R, ifobj_gdrive)
GetBack(process_query_nnalign, ifobj_gdrive)
GetBack(process_kill_nnalign, ifobj_gdrive)

load("test/ifobj_gdrive.RData")
data("iris")
taskobj <- filter(iris, Sepal.Length > 4 & Sepal.Length < 4.5) |>
  PackIn(objects = "iris", libraries = c("dplyr")) |>
  SendOut(ifobj_gdrive)

GetBack(taskobj, interface = ifobj_gdrive)
