library(rlang)
library(callr)
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
      
      resobj <- readRDS(paste0(configs$tmpdir, "/", outobj$task_id))
      if(!is.null(resobj$errors)) {return(stop(resobj$errors))} else {return(resobj$output_value)}
    }
  }
}


#################### INTERFACE ----------
# interface_gdrive: all in one function for communication with Google Drive.
interface_gdrive <- function(cmd, obj = NULL, task_id = NULL) {
  require(googledrive)
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
    tmppath <- paste0(configs$tmpdir, "/", task_id)
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
  new_tasks_appeared <- FALSE
  ongoing_tasks <- NULL
  
  while(TRUE) {
    if(!new_tasks_appeared) {
      print(paste0(Sys.time(), ' - Checking for new task or result...'))
      Sys.sleep(configs$timeout)
      new_tasks_appeared <- interface_gdrive("check_tasks")
      
      new_results <- intersect(list.files(configs$tmpdir), ongoing_tasks)
      if(length(new_results) > 0) {
        interface_gdrive("send_result", task_id = new_results[1])
        ongoing_tasks <- ongoing_tasks[-which(ongoing_tasks == new_results[1])]
      }
    } else {
      # getting and loading task obj, and removing RDS file
      task_pkg_info <- interface_gdrive("get_task")
      task_obj <- readRDS(file = task_pkg_info[1, 2][[1]])
      file.remove(task_pkg_info[1, 2][[1]])
      
      # adding configs to task_obj, so r subprocess can read information from it
      task_obj$configs <- configs

      # evaluating task obj
      ongoing_tasks <- c(ongoing_tasks, task_obj$task_id)
      procobj <- r_bg(eval_task_obj, args = list(task_obj))
      
      
      # remove task object from Google Drive
      interface_gdrive("remove_task", obj = task_obj)
      
      # turn on "keep checking" again
      new_tasks_appeared <- FALSE
    }
  }
}

eval_task_obj <- function(task_obj) {
  lapply(task_obj$libs, library, character.only = TRUE)
  attach(task_obj$objslist)
  configs <- task_obj$configs
  results_obj <- list(task_id = task_obj$task_id,
                      output_value = evaluate::evaluate(task_obj$x,output_handler = evaluate::new_output_handler(value = identity))[[2]])
  saveRDS(results_obj, file = paste0(configs$tmpdir, "/", results_obj$task_id))
}

save_results_package <- function(results_obj, result_pkg_path) {saveRDS(results_obj, file = result_pkg_path)}

#################### TESTING -----------
remete_server_session()
a = sample(1:100, size = 40)
b = sample(1:100, size = 40)

send_to_remote({
  # require(Hmisc, quietly = TRUE)
  a = 1:100
  b = 1:100
  Hmisc::rcorr(a, b)
})

evaluate({
  require(Hmisc)
  a = 1:100
  b = 1:100
  rcorr(a, b)
}, output_handler = new_output_handler(value = identity))

kimeneti_objektum_1 <- send_to_remote(rcorr(a, b), objs = c("a", "b"), libs = c("Hmisc"))
kimeneti_objektum_2 <- send_to_remote(rcorr(a, d), objs = c("a", "b"), libs = c("Hmisc"))
