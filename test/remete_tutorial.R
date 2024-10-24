# loading the library -----------
library(remete)

# creating an interface object -----------
ifobj_gdrive <- generate_interface_gdrive(server_id = "tutorial2", tmp_dir = "~/tmp")
save(ifobj_gdrive, file = "test/ifobj_gdrive_tutorial2.RData")

# in the browser: let's check the folders the previous function call has created -----------

# in the remote computer: start the remote server -----------

# task: 40 + 2 on a remote server -----------
load("test/ifobj_gdrive_tutorial2.RData")
taskobj <- PackIn(40+2) |> SendOut(ifobj_gdrive)

GetBack(taskobj, ifobj_gdrive)

# task: perform predictions on a remote server -----------
library(emhace.tools)
aas_unq <- protr::AABLOSUM62 |> rownames()
peps_unq <- sapply(1:100, \(i) sample(aas_unq, size = 9) |> paste0(collapse = ""))

taskobj <- PackIn({
  emhace.tools::RunNetMHCpan_ng(alleles = "HLA-A02:01",
                                peptides = peps_unq,
                                version_number = "4.1",
                                software_path = "/home/lhgergo/Programok/netMHCpan-4.1/netMHCpan")
  }, objects = c("peps_unq"), libraries = c("emhace.tools"), task_id = "prediction_for_tutorial") |>
  SendOut(interface = ifobj_gdrive)

GetBack(taskobj, ifobj_gdrive)

