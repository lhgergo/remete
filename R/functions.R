# Remete v0.2: complete reconceptualization of the package

# INTERFACE ----------
# generate_interface_file: generates an interface for in-computer offline testing of the package
# IMPORTANT: generate_interace_file has not been updated for a while, has to be synchronized with the generate_interface_gdrive
generate_interface_file <- function(server_id, task_dir, result_dir, tmp_dir) {
  server_id <- server_id
  task_dir <- task_dir
  result_dir <- result_dir
  tmp_dir <- tmp_dir

  function(cmd, x = NULL) {
    if(cmd == "show_configuration") {
      return(c(server_id = server_id, task_dir = task_dir, result_dir = result_dir, tmp_dir = tmp_dir))
    } else {
      task_dir <- paste0(task_dir, "/", server_id, "/")
      result_dir <- paste0(result_dir, "/", server_id, "/")
      dir.create(task_dir, recursive = T)
      dir.create(result_dir, recursive = T)
    }

    if(cmd == "send_task_package") {
      return(file.copy(from = x, to = task_dir))
    }
    if(cmd == "check_task_packages") {
      task_pkgs <- list.files(task_dir)
      return(ifelse(length(task_pkgs) > 0, 1, 0))
    }
    if(cmd == "list_task_packages") {
      return(list.files(task_dir))
    }
    if(cmd == "get_task_package") {
      file.copy(from = paste0(task_dir, "/", x), to = tmp_dir)
      return(paste0(tmp_dir, "/", x))
    }
    if(cmd == "send_result_package") {
      return(file.copy(from = x, to = result_dir))
    }
    if(cmd == "list_result_packages") {
      return(list.files(result_dir))
    }
    if(cmd == "get_result_package") {
      file.copy(from = paste0(result_dir, "/", x), to = tmp_dir)
    }
    if(cmd == "remove_task_package") {
      file.remove(paste0(task_dir, "/", x))
    }
    if(cmd == "remove_result_package") {
      file.remove(paste0(result_dir, "/", x))
    }
  }
}

# generate_interface_gdrive: generates an interface for online task outsourcing using a Google Drive storage as a mediator
generate_interface_gdrive <- function(server_id, task_dir, result_dir, file_dir, tmp_dir) {
  server_id <- server_id
  task_dir <- task_dir
  result_dir <- result_dir
  file_dir <- file_dir
  tmp_dir <- tmp_dir

  task_dir <- paste0(task_dir, "_", server_id)
  result_dir <- paste0(result_dir, "_", server_id)
  file_dir <- paste0(file_dir, "_", server_id)

  # initializing directories
  filesdf <- googledrive::drive_ls()
  if(!task_dir %in% filesdf$name) googledrive::drive_mkdir(name = task_dir, path = "")
  if(!result_dir %in% filesdf$name) googledrive::drive_mkdir(name = result_dir, path = "")
  if(!file_dir %in% filesdf$name) googledrive::drive_mkdir(name = file_dir, path = "")

  task_dir <- paste0(task_dir, "/")
  result_dir <- paste0(result_dir, "/")
  file_dir <- paste0(file_dir, "/")
  tmp_dir <- paste0(tmp_dir, "/")

  function(cmd, x) {
    if(cmd == "show_configuration") {
      return(c(server_id = server_id, task_dir = task_dir, result_dir = result_dir, file_dir = file_dir, tmp_dir = tmp_dir))
    }
    if(cmd == "send_task_package") {
      return(googledrive::drive_upload(media = x, path = task_dir))
    }
    if(cmd == "check_task_packages") {
      task_pkgs <- googledrive::drive_ls(path = task_dir)
      return(ifelse(nrow(task_pkgs) > 0, 1, 0))
    }
    if(cmd == "list_task_packages") {
      return(googledrive::drive_ls(path = task_dir)$name)
    }
    if(cmd == "get_task_package") {
      googledrive::drive_download(file = paste0(task_dir, x),
                                  path = paste0(tmp_dir, x),
                                  overwrite = TRUE)
      return(paste0(tmp_dir, "/", x))
    }
    if(cmd == "send_result_package") {
      return(googledrive::drive_upload(media = paste0(tmp_dir, "/", x),
                                       path = paste0(result_dir, x)))
    }
    if(cmd == "list_result_packages") {
      return(googledrive::drive_ls(path = result_dir))
    }
    if(cmd == "get_result_package") {
      googledrive::drive_download(file = paste0(result_dir, x),
                                  path = paste0(tmp_dir, x),
                                  overwrite = TRUE)
    }
    if(cmd == "remove_task_package") {
      googledrive::drive_rm(paste0(task_dir, x))
    }
    if(cmd == "remove_result_package") {
      googledrive::drive_rm(paste0(result_dir, x))
    }
    if(cmd == "prepare_directories") {
      googledrive::drive_mkdir(result_dir)
      googledrive::drive_mkdir(task_dir)
    }
    if(cmd == "send_file") {
      return(googledrive::drive_upload(media = x, path = file_dir))
    }
    if(cmd == "get_file") {
      dir.create(paste0(tmp_dir, file_dir), showWarnings = FALSE, recursive = T)
      googledrive::drive_download(file = paste0(file_dir, x),
                                  path = paste0(tmp_dir, file_dir, x),
                                  overwrite = TRUE)
    }
  }
}

# CLIENT ----------
# GenerateTaskID: generates a task ID number
GenerateID <- function(n = 10) paste0(sample(c(0:9, letters), size = n, replace = TRUE), collapse = "")

# PackIn: creates a task package with a unique identifier to be sent to the server
PackIn <- function(expr, objects = NULL, libraries = NULL, task_id = NULL, tmp_dir = "~/tmp/") {
  # preparing task id
  if(is.null(task_id)) task_id <- GenerateID(n = 4)
  crnt_timepoint <- gsub(" ", "-", Sys.time())
  crnt_timepoint <- gsub("\\:", "-", crnt_timepoint)
  task_id <- paste0(crnt_timepoint, "-", task_id)

  # converting expression to expression format
  expr <- rlang::enexpr(expr)

  objslist = if(!is.null(objects)) as.environment(mget(objects, envir = .GlobalEnv)) # creates object environment
  task_package <- list(task_id = task_id, expr = expr, objects = objslist,
                       libraries = libraries) # creates task object

  dir.create(tmp_dir, showWarnings = FALSE, recursive = T); task_package_path <- paste0(tmp_dir, task_id) # saves task package path on local computer
  save(task_package, file = task_package_path)
  return(c(task_id = task_id,
           task_package_path = task_package_path,
           target_session = NULL))
}

# SendOut: sends the newly created package via a chosen protocol to a remote serve
SendOut <- function(task_pack, interface) {
  eval(rlang::call2(interface, cmd = "send_task_package", x = task_pack["task_package_path"]))
  message(paste0("Task ", task_pack["task_id"]), " has been sent.")
  return(c(task_pack, interface = interface))
}

# GetBack: gets the result back from the interface
GetBack <- function(task_package_info, interface = NULL, simplified_output = TRUE) {
  if(is.null(interface)) interface <- task_package_info["interface"]
  eval(rlang::call2(interface, cmd = "get_result_package", x = task_package_info["task_id"]))

  tmpdf <- interface("show_configuration")["tmp_dir"]
  # waiting for the arrival of result package
  Sys.sleep(1)
  result_received <- task_package_info["task_id"] %in% list.files(tmpdf)
  while(!result_received) {
    Sys.sleep(1)
    result_received <- task_package_info["task_id"] %in% list.files(tmpdf)
  }

  load(paste0(tmpdf, "/", task_package_info["task_id"]))
  message(paste0("Result package for task ", results_package$task_id, " has been received."))
  eval(rlang::call2(interface, cmd = "remove_result_package", x = task_package_info["task_id"]))
  message(paste0("Result package for task ", results_package$task_id, " has been removed from the interface."))

  if(simplified_output) {
    return(results_package$output_value[[length(results_package$output_value)]])
  } else {
    return(results_package$output_value)
  }
}


# SendFile: transfers a file from a targetted site on the remote computer
SendFile <- function(from, to, interface, tmp_dir = "~/tmp/") {
  # sending file to the interface
  eval(rlang::call2(interface, cmd = "send_file", x = from))

  # creating a targetting label, uploading it also
  write(to, file = paste0(tmpdir))
  eval(rlang::call2(interface, cmd = "send_file", x = from))
}



# SERVER ----------
RunServer <- function(interface) {
  new_task <- FALSE

  while(TRUE) {
    Sys.sleep(1)
    if(!new_task) {
      new_task <- eval(rlang::call2(interface, cmd = "check_task_packages"))
    } else  {
      # downloading the first task package
      crnt_task <- eval(rlang::call2(interface, cmd = "list_task_packages"))[1]
      message(paste0(Sys.time(), " - received task ", crnt_task, ", evaluating."))
      task_package_path <- eval(rlang::call2(interface, cmd = "get_task_package", x = crnt_task))
      task_package_name <- tail(unlist(strsplit(task_package_path, "/")), 1)

      # running the task package, adding process package to the processes directory, removing task package from the interface and the tmp directory
      tmp_dir <- eval(rlang::call2(interface, cmd = "show_configuration"))["tmp_dir"]
      callr::r(EvaluateTaskPackage, args = list(task_package_path, tmp_dir))
      message(paste0(Sys.time(), " - finished task ", crnt_task, "."))
      eval(rlang::call2(interface, cmd = "remove_task_package", x = crnt_task))

      # sending the result package
      eval(rlang::call2(interface, cmd = "send_result_package", x = task_package_name))

      new_task <- FALSE
    }
  }
}

EvaluateTaskPackage <- function(task_package_path, tmp_dir) {
  load(task_package_path)
  file.remove(task_package_path)
  lapply(task_package$libraries, function(x) {library(x, logical.return = TRUE, character.only = TRUE)})
  attach(task_package$objects)
  results_package <- list(task_id = task_package$task_id,
                          output_value = evaluate::evaluate(task_package$expr, output_handler = evaluate::new_output_handler(value = identity), keep_warning = FALSE))
  save(results_package, file = paste0(tmp_dir, "/", results_package$task_id))
}

# experimental asynchronous version of the remete server
RunServerAsync <- function(interface) {
  new_task <- FALSE
  ongoing_tasks <- list()
  tmp_dir <- eval(rlang::call2(interface, cmd = "show_configuration"))["tmp_dir"]

  while(TRUE) {
    Sys.sleep(1)
    if(!new_task) {
      new_task <- eval(rlang::call2(interface, cmd = "check_task_packages"))

      # checking what is the situation with ongoing tasks
      if(length(ongoing_tasks) > 0) {
        ready_tasks <- intersect(names(ongoing_tasks), list.files(tmp_dir))

        if(length(ready_tasks) > 0) {
          for (crnt_task in ready_tasks) {
            message(paste0(Sys.time(), " - finished task ", crnt_task, "."))
            eval(rlang::call2(interface, cmd = "send_result_package", x = crnt_task))
            ongoing_tasks[[crnt_task]] <- NULL
          }
        }
      }
    } else  {
      # downloading the first task package
      crnt_task <- eval(rlang::call2(interface, cmd = "list_task_packages"))[1]
      message(paste0(Sys.time(), " - received task ", crnt_task, ", evaluating."))
      task_package_path <- eval(rlang::call2(interface, cmd = "get_task_package", x = crnt_task))
      task_package_name <- tail(unlist(strsplit(task_package_path, "/")), 1)

      # running the task package, adding process package to the processes directory, removing task package from the interface and the tmp directory
      ongoing_tasks[[task_package_name]] <- callr::r_bg(EvaluateTaskPackage, args = list(task_package_path, tmp_dir))
      eval(rlang::call2(interface, cmd = "remove_task_package", x = crnt_task))

      new_task <- FALSE
    }
  }
}
