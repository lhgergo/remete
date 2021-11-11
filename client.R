library(rlang)
library(magrittr)
library(googledrive)
library(future) #  for asynchronous task evaluation

#################### CONFIGURATION ----------
# This should be stored somehow in a file. For now I will just keep them here.
configs <- list()
configs$drive_dir <- "remete/"
configs$tmpdir <- "~/tmp/"
dir.create(configs$tmpdir)

#################### CLIENT ----------
# send_to_remote: generates task package and passes to the selected interface
# x: expression
# objs: objects to export
# libs: libraries to load on the remote computer
# interface: interface function to be used to send data

send_to_remote <- function(x, objs = NULL, libs = NULL, task_id = NULL, interface = interface_gdrive_send_task) {
  task_id <- ifelse(is.null(task_id), paste0(sample(c(0:9, letters), size = 10, replace = TRUE), collapse = ""), task_id)
  x <- enexpr(x)
  objslist = as.environment(mget(objs, envir = .GlobalEnv))
  outobj <- list(task_id = task_id, x = x, objslist = objslist, libs = libs)
  call2(interface, list(task_id = task_id, x = x, objslist = objslist, libs = libs)) %>% eval()
}

#################### INTERFACE ----------
# interface_gdrive_initialize: building connection with the appropriate Google Drive account to perform remote control.
interface_gdrive_initialize <- function(remete_dir = "remete/") {
  drive_auth()
  drive_mkdir(remete_dir, overwrite = TRUE)
}

# interface_gdrive_send: sends task package to the cloud. Being run by the client.
interface_gdrive_send_task <- function(obj) {
  tmppath <- paste0(configs$tmpdir, "/", obj$task_id)
  saveRDS(obj, file = tmppath)
  drive_upload(media = tmppath, path = configs$drive_dir)
}

# interface_gdrive_check: checks for newly available task packages in the cloud. Being run by the server.
interface_gdrive_check <- function(remete_dir = configs$drive_dir) {
  task_pkgs <- drive_ls(remete_dir)
  ifelse(nrow(task_pkgs) > 0, 1, 0)
}

# interface_gdrive_get: downloads the latest, yet unprocessed task package from the cloud.
interface_gdrive_get_task <- function(remete_dir = configs$drive_dir) {
  newest_task_pkg <- drive_ls(remete_dir)[1, ]
  drive_download(file = newest_task_pkg[1, ], path = paste0(configs$tmpdir, "/", as.vector(newest_task_pkg[1, 1])), overwrite = TRUE)
}

# interface_gdrive_send_result: sends the processed results back to the client
# interface_gdrive_get_result: downloads the processed results back to the client

#################### SERVER ----------
# remete_server_session: the main function orchestrating remete tasks
# assign_task: loading 
remete_server_session <- function(timeout = 5) {
  keep_checking <- TRUE
  while(TRUE) {
    if(keep_checking) {
      beepr::beep(1)
      Sys.sleep(timeout)
      keep_checking <- !interface_gdrive_check()
    } else {
      task_pkg_info <- interface_gdrive_get_task()
      task_obj <- readRDS(file = task_pkg_info[1, 2][[1]])
      results_obj <- eval_task_obj(task_obj)
      return(results_obj)
      # respath <- paste0(task_pkg_info[1, 2][[1]], "_result")
      # save_results_package(results_obj, file = respath)
      # interface_gdrive_send_result()
    }
  }
}

eval_task_obj <- function(obj) {
  tmpenv <- obj$objslist
  attach(tmpenv)
  ftrobj <- future(eval(obj$x), packages = obj$libs)
  value(ftrobj)
}

save_results_package <- function(results_obj, result_pkg_path) {saveRDS(results_obj, file = result_pkg_path)}

#################### TESTING -----------
interface_gdrive_initialize()
remete_server_session()
a = 1:10
b = 2:11
send_to_remote(rcorr(a, b), objs = c("a", "b"), libs = c("Hmisc"))

