library(magrittr)
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

taskobj <- PackIn(sample(1:100, 50)) %>% SendOut(ifobj_gdrive)
GetBack(taskobj, interface = ifobj_gdrive, simplified_output = T)
