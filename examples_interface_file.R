# running server ----------
RunServer(interface = "interface_gdrive")

# experiment with async run
taskobj <- PackIn('3+2', task_id = "1234") %>% SendAway(interface = "interface_gdrive")
GetBack(taskobj)



##############################

library(emhace.tools)
load("../SARS-CoV-2-blindspot_data/selfsim_dissim_calculation/seqsims.RData")
peps <- seqsims %>% names() %>% extract(1:10)
taskpack <- PackIn(RunNetMHCpan(alleles = c("HLA-A02:01", "HLA-A03:01"), peptides = peps,
                                software_path = "/home/lhgergo/Programok/netMHCpan-4.1/netMHCpan", output_format = "wide"),
                   objects = c("peps"),
                   libraries = c("emhace.tools")) %>% SendAway(interface = "interface_file")

GetBack(taskpack, interface = "interface_file")
GetBack(c(task_id = "2023-01-18-17-42-49-nr92", interface = "interface_file"))
taskpack2 <- PackIn(4 + 3,
                    objects = c("peps"),
                    libraries = c("emhace.tools")) %>% SendAway(interface = "interface_file")

interface_file(cmd = "list_result_packages")

GetBack(taskpack2)

ClearDirs <- function() {
  c(list.files(configs_remete$interface_file_task_dir, full.names = TRUE),
    list.files(configs_remete$interface_file_result_dir, full.names = TRUE),
    list.files(configs_remete$tmpdir, full.names = TRUE)) %>% file.remove()
}

ClearDirs()

# server side
RunServer(session_id = "lajos", interface = "interface_file")

peps <- c("ALALALSLAL", "AALSLALSA")
taskpack <- PackIn(RunNetMHCpan(alleles = c("HLA-A02:01", "HLA-A03:01"), peptides = peps,
                                software_path = "/home/lhgergo/Programok/netMHCpan-4.1/netMHCpan", output_format = "wide"),
                   objects = c("peps"), libraries = c("emhace.tools")) %>% SendAway(interface = "interface_file")
GetBack(taskpack)

taskpack <- PackIn(RunServer(), libraries = c("remete")) %>% SendAway(interface = "interface_file")
taskpack <- PackIn(3+4, libraries = c("remete")) %>% SendAway(interface = "interface_file")
