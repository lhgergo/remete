library(rlang)
library(callr)
library(magrittr)
library(googledrive)
library(evaluate)
library(tibble)
library(future) #  for asynchronous task evaluation

#################### CONFIGURATION ----------
# This should be stored somehow in a file. For now I will just keep them here.
configs_remete <- list()
configs_remete$drive_task_dir <- "remete_tasks/"
configs_remete$drive_results_dir <- "remete_results/"
configs_remete$tmpdir <- "~/tmp/"
configs_remete$timeout <- 1
dir.create(configs_remete$tmpdir)

#################### CLIENT ----------
# send_to_remote: generates task package and passes to the selected interface
# x: expression
# objs: objects to export
# libs: libraries to load on the remote computer
# task_id: a pre-determined ID for the task. If NULL, generates a random ID of length 10
# interface: interface function to be used to send data

send_to_remote <- function(x, objs = NULL, libs = NULL, task_id = NULL, 
                           interface = "interface_gdrive", wait_for_result = TRUE) {
  task_id <- ifelse(is.null(task_id), paste0(sample(c(0:9, letters), size = 10, replace = TRUE), collapse = ""), task_id)
  x <- enexpr(x) # converting expression
  objslist = if(!is.null(objs)) as.environment(mget(objs, envir = .GlobalEnv)) # creates object environment
  outobj <- list(task_id = task_id, x = x, objslist = objslist, libs = libs) # creates task object
  call2(interface, "send_task", list(task_id = task_id, x = x, objslist = objslist, libs = libs, keep_in_cloud = !wait_for_result)) %>% eval() # sending task
  
  if(wait_for_result) {
    # waiting for response
    responded <- FALSE
    while(!responded) {
      Sys.sleep(configs_remete$timeout)
      resfiles_in_cloud <- call2(interface, cmd = "list_result_pkgs") %>% eval()
      if(outobj$task_id %in% resfiles_in_cloud) {
        call2(interface, "get_result", task_id = task_id) %>% eval()
        call2(interface, "remove_result", task_id = task_id) %>% eval()
        
        resobj <- readRDS(paste0(configs_remete$tmpdir, "/", outobj$task_id))
        if(!is.null(resobj$errors)) {return(stop(resobj$errors))} else {return(resobj$output_value)}
      }
    }
  } else {
    # don't wait for response
    message(paste("Task", task_id , "had been passed to the interface"))
  }
}

# load_result: downloads and loads result object from the cloud
load_result <- function(task_id, interface, remove_from_cloud = TRUE) {
  call2(interface, "get_result", task_id = task_id) %>% eval()
  if(remove_from_cloud) {call2(interface, "remove_result", task_id = task_id) %>% eval()}
  resobj <- readRDS(paste0(configs_remete$tmpdir, "/", task_id))
  file.remove(paste0(configs_remete$tmpdir, "/", task_id))
  return(resobj$output_value)
}



#################### INTERFACE ----------
# interface_gdrive: all in one function for communication with Google Drive.
interface_gdrive <- function(cmd, obj = NULL, task_id = NULL) {
  require(googledrive)
  if(cmd == "send_task") {
    tmppath <- paste0(configs_remete$tmpdir, "/", obj$task_id)
    saveRDS(obj, file = tmppath)
    return(drive_upload(media = tmppath, path = configs_remete$drive_task_dir))
  }
  if(cmd == "check_tasks") {
    task_pkgs <- drive_ls(configs_remete$drive_task_dir)
    return(ifelse(nrow(task_pkgs) > 0, 1, 0))
  }
  if(cmd == "list_result_pkgs") {
    return(drive_ls(configs_remete$drive_results_dir)$name)
  }
  if(cmd == "get_task") {
    newest_task_pkg <- drive_ls(configs_remete$drive_task_dir)[1, ]
    return(drive_download(file = newest_task_pkg[1, ], path = paste0(configs_remete$tmpdir, "/", as.vector(newest_task_pkg[1, 1])), overwrite = TRUE))
  }
  if(cmd == "send_result") {
    tmppath <- paste0(configs_remete$tmpdir, "/", task_id)
    return(drive_upload(media = tmppath, path = configs_remete$drive_results_dir))
  }
  if(cmd == "get_result") {
    result_pkgs <- drive_ls(configs_remete$drive_results_dir)
    result_pkgs <- result_pkgs[which(result_pkgs$name == task_id), ]
    return(drive_download(file = result_pkgs, path = paste0(configs_remete$tmpdir, "/", as.vector(result_pkgs[1, 1])), overwrite = TRUE))
  }
  if(cmd == "remove_task") {
    drive_rm(paste0(configs_remete$drive_task_dir, task_id))
  }
  if(cmd == "remove_result") {
    drive_rm(paste0(configs_remete$drive_results_dir, task_id))
  }
}

#################### SERVER ----------
# remete_server_session: the main function orchestrating remete tasks
# assign_task: loading 
remete_server_session <- function(interface = "interface_gdrive") {
  no_unprocessed_task <- TRUE
  ongoing_tasks <- tibble(task_id = vector(mode = "character"),
                          status = vector(mode = "character"),
                          keep_in_cloud = vector(mode = "logical"))
  
  while(TRUE) {
    if(no_unprocessed_task) {
      print(paste0(Sys.time(), ' - Checking for new task or result...'))
      Sys.sleep(configs_remete$timeout)
      new_tasks_appeared <- call2(interface, "check_tasks") %>% eval() # sending task
      
      new_result_objs <- intersect(list.files(configs_remete$tmpdir), ongoing_tasks$task_id)
      if(length(new_result_objs) > 0) {
        new_result_obj <- new_result_objs[1]
        keep_new_result_in_cloud <- ongoing_tasks$keep_in_cloud[ongoing_tasks$task_id == new_result_obj]
        call2(interface, "send_result", task_id = new_result_obj) %>% eval() # sending result
        
        if(keep_new_result_in_cloud) {
          ongoing_tasks[ongoing_tasks$task_id == new_result_obj, "status"] <- "uploaded to cloud"
        } else {
          # if the output is to be processed immediately by the client, remove task
          ongoing_tasks <- ongoing_tasks[-which(ongoing_tasks$task_id == new_results[1]), ]
        }
      }
      
      if(any(ongoing_tasks$keep_in_cloud)) {
        result_pkg_already_downloaded <- which(ongoing_tasks$keep_in_cloud & !ongoing_tasks$task_id %in% interface_gdrive("list_result_pkgs"))
        if(length(result_pkg_already_downloaded) > 0) ongoing_tasks <- ongoing_tasks[-result_pkg_already_downloaded, ]
      }
      
    } else {
      # getting and loading task obj, and removing RDS file
      task_pkg_info <- call2(interface, "get_task") %>% eval()
      task_obj <- readRDS(file = task_pkg_info[1, 2][[1]])
      file.remove(task_pkg_info[1, 2][[1]])
      
      # adding configs_remete to task_obj, so r subprocess can read information from it
      task_obj$configs_remete <- configs_remete

      # evaluating task obj
      procobj <- r_bg(eval_task_obj, args = list(task_obj))
      ongoing_tasks <- rbind(ongoing_tasks, tibble(task_id = task_obj$task_id,
                                                   status = "in progress",
                                                   keep_in_cloud = task_obj$keep_in_cloud))
      
      # remove task object from Google Drive
      call2(interface, "remove_task", task_id = task_obj$task_id) %>% eval()
      
      # turn on "keep checking" again
      no_unprocessed_task <- TRUE
    }
  }
}

eval_task_obj <- function(task_obj) {
  lapply(task_obj$libs, library, character.only = TRUE)
  attach(task_obj$objslist)
  configs_remete <- task_obj$configs_remete
  results_obj <- list(task_id = task_obj$task_id,
                      output_value = evaluate::evaluate(task_obj$x,output_handler = evaluate::new_output_handler(value = identity), keep_warning = FALSE)[[2]],
                      keep_in_cloud = task_obj$keep_in_cloud)
  saveRDS(results_obj, file = paste0(configs_remete$tmpdir, "/", results_obj$task_id))
}

save_results_package <- function(results_obj, result_pkg_path) {saveRDS(results_obj, file = result_pkg_path)}

# #################### TESTING -----------
# remete_server_session(interface = "interface_gdrive")
# a = sample(1:100, size = 40)
# b = sample(1:100, size = 40)
# 
send_to_remote({
  a = 1:100
  b = 1:100
  rcorr(a, b)
}, libs = c("Hmisc"), wait_for_result = FALSE)
# 
# 
# kimeneti_objektum_1 <- send_to_remote(rcorr(a, b), objs = c("a", "b"), libs = c("Hmisc"), wait_for_result = FALSE)
# load_result("js4ugyb7kk", "interface_gdrive", remove_from_cloud = FALSE)
# 
# 
# ############### trying out a netmhcpan run
# list.files("~/Dokumentumok/LABOR/SARS-CoV-2-immunoadaptation/netMHCpan-tools_functions/", full.names = TRUE) %>%
#   extract(!grepl("db_playground", .)) %>% lapply(source)
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
