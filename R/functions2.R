# Remete v0.2: complete reconceptualization of the package

# CONFIGURATIONS ----------
library(magrittr)
configs_remete <- rjson::fromJSON(file = "~/remete_configs.json")
configs_remete$interface_file_task_dir <- "~/remete_tasks/"
configs_remete$interface_file_result_dir <- "~/remete_results/"
configs_remete$ongoing_processes_dir <- "~/remete_processes/"
configs_remete$interface_gdrive_task_dir <- "remete_tasks/"
configs_remete$interface_gdrive_result_dir <- "remete_results/"

# INTERFACE ----------
# interface_file: an interface for in-computer offline testing of the package
interface_file <- function(cmd, x) {
  if(cmd == "send_task_package") {
    return(file.copy(from = x, to = configs_remete$interface_file_task_dir))
  }
  if(cmd == "check_task_packages") {
    task_pkgs <- list.files(configs_remete$interface_file_task_dir)
    return(ifelse(length(task_pkgs) > 0, 1, 0))
  }
  if(cmd == "list_task_packages") {
    return(list.files(configs_remete$interface_file_task_dir))
  }
  if(cmd == "get_task_package") {
    file.copy(from = paste0(configs_remete$interface_file_task_dir, "/", x), to = configs_remete$tmpdir)
    return(paste0(configs_remete$tmpdir, "/", x))
  }
  if(cmd == "send_result_package") {
    return(file.copy(from = x, to = configs_remete$interface_file_result_dir))
  }
  if(cmd == "list_result_packages") {
    return(list.files(configs_remete$interface_file_result_dir))
  }
  if(cmd == "get_result_package") {
    file.copy(from = paste0(configs_remete$interface_file_result_dir, "/", x), to = configs_remete$tmpdir)
  }
  if(cmd == "remove_task_package") {
    file.remove(paste0(configs_remete$interface_file_task_dir, "/", x))
  }
  if(cmd == "remove_result_package") {
    file.remove(paste0(configs_remete$interface_file_result_dir, "/", x))
  }
}

# interface_gdrive: an interface for online task outsourcing using a Google Drive storage as a mediator
interface_gdrive <- function(cmd, x) {
  if(cmd == "send_task_package") {
    return(googledrive::drive_upload(media = x, path = configs_remete$interface_gdrive_task_dir))
  }
  if(cmd == "check_task_packages") {
    task_pkgs <- googledrive::drive_ls(path = configs_remete$interface_gdrive_task_dir)
    return(ifelse(nrow(task_pkgs) > 0, 1, 0))
  }
  if(cmd == "list_task_packages") {
    return(googledrive::drive_ls(path = configs_remete$interface_gdrive_task_dir)$name)
  }
  if(cmd == "get_task_package") {
    googledrive::drive_download(file = paste0(configs_remete$interface_gdrive_task_dir, x),
                                path = paste0(configs_remete$tmpdir, x),
                                overwrite = TRUE)
    return(paste0(configs_remete$tmpdir, "/", x))
  }
  if(cmd == "send_result_package") {
    googledrive::drive_upload(media = paste0(configs_remete$tmpdir, "/", x),
                              path = paste0(configs_remete$interface_gdrive_result_dir, x))
  }
  if(cmd == "list_result_packages") {
    return(googledrive::drive_ls(path = configs_remete$interface_gdrive_result_dir))
  }
  if(cmd == "get_result_package") {
    googledrive::drive_download(file = paste0(configs_remete$interface_file_result_dir, x),
                                path = paste0(configs_remete$tmpdir, x),
                                overwrite = TRUE)
  }
  if(cmd == "remove_task_package") {
    googledrive::drive_rm(paste0(configs_remete$interface_gdrive_task_dir, x))
  }
  if(cmd == "remove_result_package") {
    googledrive::drive_rm(paste0(configs_remete$interface_gdrive_result_dir, x))
  }
}


# CLIENT ----------
# GenerateTaskID: generates a task ID number
GenerateID <- function(n = 10) paste0(sample(c(0:9, letters), size = n, replace = TRUE), collapse = "")

# PackIn: creates a task package with a unique identifier to be sent to the server
PackIn <- function(expr, objects = NULL, files = NULL, libraries = NULL, task_id = NULL) {
  # preparing task id
  if(is.null(task_id)) task_id <- GenerateID(n = 4)
  crnt_timepoint <- gsub(" ", "-", Sys.time())
  crnt_timepoint <- gsub("\\:", "-", crnt_timepoint)
  task_id <- paste0(crnt_timepoint, "-", task_id)

  # converting expression to expression format
  expr <- rlang::enexpr(expr)

  objslist = if(!is.null(objects)) as.environment(mget(objects, envir = .GlobalEnv)) # creates object environment
  task_package <- list(task_id = task_id, expr = expr, objects = objslist, libraries = libraries) # creates task object

  task_package_path <- paste0(configs_remete$tmpdir, "/", task_id) # saves task package path on local computer
  save(task_package, file = task_package_path)
  return(c(task_id = task_id,
           task_package_path = task_package_path,
           target_session = NULL))
}

# SendOut: sends the newly created package via a chosen protocol to a remote serve
SendAway <- function(task_package_info, interface, target_session = NULL) {
  eval(rlang::call2(interface, cmd = "send_task_package", x = task_package_info["task_package_path"]))
  message(paste0("Task ", task_package_info["task_id"]), " has been sent to ", interface)
  return(c(task_package_info, interface = interface))
}

# GetBack: gets the result back from the interface (TODO!)
GetBack <- function(task_package_info, interface = NULL) {
  if(is.null(interface)) interface <- task_package_info["interface"]
  eval(rlang::call2(interface, cmd = "get_result_package", x = task_package_info["task_id"]))

  # waiting for the arrival of result package
  Sys.sleep(1)
  result_received <- task_package_info["task_id"] %in% list.files(configs_remete$tmpdir)
  while(!result_received) {
    Sys.sleep(1)
    result_received <- task_package_info["task_id"] %in% list.files(configs_remete$tmpdir)
  }

  load(paste0(configs_remete$tmpdir, "/", task_package_info["task_id"]))
  message(paste0("Result package for task ", results_package$task_id, " has been received."))
  eval(rlang::call2(interface, cmd = "remove_result_package", x = task_package_info["task_id"]))
  message(paste0("Result package for task ", results_package$task_id, " has been removed from the interface."))
  return(results_package$output_value)
}

# SERVER ----------
RunServer <- function(session_id = NULL, interface = "interface_file") {
  if(is.null(session_id)) session_id <- GenerateID(4)
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
      callr::r(EvaluateTaskPackage, args = list(task_package_path, configs_remete))
      message(paste0(Sys.time(), " - finished task ", crnt_task, "."))
      eval(rlang::call2(interface, cmd = "remove_task_package", x = crnt_task))

      # sending the result package
      eval(rlang::call2(interface, cmd = "send_result_package", x = task_package_name))

      new_task <- FALSE
    }
  }
}

EvaluateTaskPackage <- function(task_package_path, configs_remete) {
  load(task_package_path)
  lapply(task_package$libraries, function(x) {library(x, logical.return = TRUE, character.only = TRUE)})
  attach(task_package$objects)
  results_package <- list(task_id = task_package$task_id,
                          output_value = evaluate::evaluate(task_package$expr, output_handler = evaluate::new_output_handler(value = identity), keep_warning = FALSE)[[2]])
  save(results_package, file = paste0(configs_remete$tmpdir, "/", results_package$task_id))
}

# experimental asynchronous version of the remete server
RunServerAsync <- function(session_id = NULL, interface = "interface_file") {
  if(is.null(session_id)) session_id <- GenerateID(4)
  new_task <- FALSE

  ongoing_tasks <- new.env(parent = emptyenv()) # an empty environment for future r_bg output objects

  while(TRUE) {
    Sys.sleep(1)
    if(!new_task) {
      new_task <- eval(rlang::call2(interface, cmd = "check_task_packages"))
    } else  {
      # downloading the first task package
      crnt_task <- eval(rlang::call2(interface, cmd = "list_task_packages"))[1]
      message(paste0(Sys.time(), " - received task ", crnt_task, ", evaluating."))
      task_package_path <- eval(rlang::call2(interface, cmd = "get_task_package", x = crnt_task))

      # running async evaluation
      # TODO: crnt_task id should be given as a name to the r_bg output object in the list
      assign(x = crnt_task, callr::r_bg(EvaluateTaskPackage, args = list(task_package_path, configs_remete)), envir = ongoing_tasks)

      # checking if any of the processes are ready
      if(length(ongoing_tasks) > 0) {
        lapply(ongoing_tasks, function(rbgobj) {
          rbgobj$read_output_lines()
        })
      }
      while(i == 0) {}

      # eval(rlang::call2(interface, cmd = "remove_task_package", x = crnt_task))

      # sending the result package
      # eval(rlang::call2(interface, cmd = "send_result_package", x = paste0(configs_remete$tmpdir, "/", crnt_task)))

      # new_task <- FALSE
    }
  }
}

# v1.0 ----------
# - kétkulcsos hitelesítés

# v0.4 ----------
# - állapot-jelentések lehetővé tétele Discordra
# - htop-szerű állapot-lekérdezés a gépről (foglalt szálak száma, szabad RAM stb.)
# - megjegyezések hozzáfűzésének lehetősége a feladatokban
# - BRC VPN/FTP interface megírása

# v0.3 ----------
# - folyamatok párhuzamos futtatásának lehetősége
# - futó folyamatok lekérdezésének lehetősége
# - ID alapján célozható legyen, pl. mely ws kapja a feladatot
# - logó elkészítése

# v0.2 ----------
# - ne legyen szükség a teljes task_objectre a feladat lekérdezéséhez, elég legyen a task_id
# - googledrive interface megírása -> PIPA

# TODOs -----------
# - lekérdezhető legyen, mely feladat-csomagokat ki küldte, mikor, és hol állnak a várólistában.
# - indítható legyen új szerver távolról!
# - VPN/FTP (BRC network) támogatás kidolgozása
# - GetBack: összepipeolhatóvá kellene tenni a SendAway-jel.
# - EGYELŐRE EBBEN A VERZIÓBAN NEM LESZ MULTITASKING KEZELÉS! de ha lesz, azt a callr:r_bg függvényével fogjuk megcsinálni, és a futó folyamatokat objektumokként mentjük majd ki.
# - ellenőriznie kell a szervernek, hogy vajon az elindított folyamat miért nem adott resultot: errorral végződött? -> a procobjokat is mentegessük valahova, hogy ezeket vissza lehessen kérdezni? -> ez lehetne a feladatkezelő alapja?

# PIPA ----------
# - A szerver nyomon tudja követni, melyik feladat mikor érkezett be. Ehhez lehet jobb lenne, ha a random jel mellett még egy időcímke is járna a package-knek.
# - GetBack függvény: ezt még át kell azért gondolni
# - az elkészült feladatot el kell távolítani a taskok közül, hogy a következőre tudjon lépni
