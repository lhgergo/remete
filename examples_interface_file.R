# running server ----------
gdrive_if <- generate_interface_gdrive(server_id = "proba", task_dir = "remete_tasks", result_dir = "remete_results", tmp_dir = "/home/lhgergo/tmp")
taskobj <- PackIn('3+2', task_id = "1234") %>% SendOut(interface = "gdrive_if")

GetBack(taskobj)


file_if <- generate_interface_file(server_id = "proba", task_dir = "~/remete_tasks/", result_dir = "~/remete_results/", tmp_dir = "/home/lhgergo/tmp")
taskobj <- PackIn('3+2', task_id = "1234") %>% SendOut(interface = file_if)
