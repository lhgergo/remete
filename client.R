library(rlang)
library(magrittr)
library(googledrive)
library(evaluate)
library(future) #  for asynchronous task evaluation

#################### CONFIGURATION ----------
# This should be stored somehow in a file. For now I will just keep them here.
configs <- list()
configs$drive_task_dir <- "remete_tasks/"
configs$drive_results_dir <- "remete_results/"
configs$tmpdir <- "~/tmp/"
configs$timeout <- 1
dir.create(configs$tmpdir)

#################### CLIENT ----------
# send_to_remote: generates task package and passes to the selected interface
# x: expression
# objs: objects to export
# libs: libraries to load on the remote computer
# interface: interface function to be used to send data

send_to_remote <- function(x, objs = NULL, libs = NULL, task_id = NULL, interface = "interface_gdrive") {
  task_id <- ifelse(is.null(task_id), paste0(sample(c(0:9, letters), size = 10, replace = TRUE), collapse = ""), task_id)
  x <- enexpr(x) # converting expression
  objslist = if(!is.null(objs)) as.environment(mget(objs, envir = .GlobalEnv)) # creates object environment
  outobj <- list(task_id = task_id, x = x, objslist = objslist, libs = libs) # creates task object
  call2(interface, "send_task", list(task_id = task_id, x = x, objslist = objslist, libs = libs)) %>% eval() # sending task
  
  # waiting for response
  responded <- FALSE
  while(!responded) {
    Sys.sleep(configs$timeout)
    resfiles_in_cloud <- call2(interface, cmd = "list_result_pkgs") %>% eval()
    if(outobj$task_id %in% resfiles_in_cloud) {
      call2(interface, "get_result") %>% eval()
      call2(interface, "remove_result", obj = outobj) %>% eval()
      return(readRDS(paste0(configs$tmpdir, "/", outobj$task_id))$output_value)
    }
  }
}


#################### INTERFACE ----------
# interface_gdrive: all in one function for communication with Google Drive.
interface_gdrive <- function(cmd, obj = NULL) {
  if(cmd == "send_task") {
    tmppath <- paste0(configs$tmpdir, "/", obj$task_id)
    saveRDS(obj, file = tmppath)
    return(drive_upload(media = tmppath, path = configs$drive_task_dir))
  }
  if(cmd == "check_tasks") {
    task_pkgs <- drive_ls(configs$drive_task_dir)
    return(ifelse(nrow(task_pkgs) > 0, 1, 0))
  }
  if(cmd == "list_result_pkgs") {
    return(drive_ls(configs$drive_results_dir)$name)
  }
  if(cmd == "get_task") {
    newest_task_pkg <- drive_ls(configs$drive_task_dir)[1, ]
    return(drive_download(file = newest_task_pkg[1, ], path = paste0(configs$tmpdir, "/", as.vector(newest_task_pkg[1, 1])), overwrite = TRUE))
  }
  if(cmd == "send_result") {
    tmppath <- paste0(configs$tmpdir, "/", paste0(obj$task_id))
    saveRDS(obj, file = tmppath)
    return(drive_upload(media = tmppath, path = configs$drive_results_dir))
  }
  if(cmd == "get_result") {
    newest_result_pkg <- drive_ls(configs$drive_results_dir)[1, ]
    return(drive_download(file = newest_result_pkg[1, ], path = paste0(configs$tmpdir, "/", as.vector(newest_result_pkg[1, 1])), overwrite = TRUE))
  }
  if(cmd == "remove_task") {
    drive_rm(paste0(configs$drive_task_dir, obj$task_id))
  }
  if(cmd == "remove_result") {
    drive_rm(paste0(configs$drive_results_dir, obj$task_id))
  }
}


#################### SERVER ----------
# remete_server_session: the main function orchestrating remete tasks
# assign_task: loading 
remete_server_session <- function() {
  keep_checking <- TRUE
  while(TRUE) {
    if(keep_checking) {
      print(paste0(Sys.time(), ' - Checking for new task...'))
      Sys.sleep(configs$timeout)
      keep_checking <- !interface_gdrive("check_tasks")
    } else {
      task_pkg_info <- interface_gdrive("get_task")
      task_obj <- readRDS(file = task_pkg_info[1, 2][[1]])
      results_obj <- eval_task_obj(task_obj)
      interface_gdrive("send_result", obj = results_obj)
      interface_gdrive("remove_task", obj = task_obj)
      keep_checking <- TRUE
    }
  }
}

eval_task_obj <- function(obj) {
  tmpenv <- obj$objslist
  attach(tmpenv)
  ftrobj <- future(eval(obj$x), packages = obj$libs)
  list(task_id = obj$task_id, output_value = value(ftrobj, signal = F))
}

save_results_package <- function(results_obj, result_pkg_path) {saveRDS(results_obj, file = result_pkg_path)}

#################### TESTING -----------
remete_server_session()
a = sample(1:100, size = 40)
b = sample(1:100, size = 40)

kimeneti_objektum <- send_to_remote(rcorr(a, b), objs = c("a", "b"), libs = c("Hmisc"))

