# remete: an R package for easy task delegation to remote computers
The aim of the project is to create a modular, R-based framework to easily send (mainly resource-heavy) tasks in R to remote workstations from client computers with lower amounts of resources (e.g. laptops, tablets). 

Note: `remete` is still under heavy development, both the underlying code and the syntax is a subject for further changes. 

# Installing
You can install the package itself using the following command:

```
devtools::install_github("lhgergo/remete")
```

# Getting started
In this section I present a step-by-step introduction for running a remete server and sending tasks to it from a client machine.

Note, that as of now (9th October, 2024), only Google Drive-based task transfer is ready. Please make sure that you have `googledrive` (https://googledrive.tidyverse.org/) package installed in R. I would like to implement further "protocols" later.

## Creating an interface
First, you have to create an *interface* object. It is a function, containing directory paths, as well as commands that functions of the package use for communication between the two sides. To create an interface object, run the `generate_interface_gdrive` command, as shown in the next code chunk. For detailed information about the directories, see the helpfile of `generate_interface_gdrive`. Note that you have to save the resulting interface object for future use on your local client computer, and to set up the remote server.

```r
ifobj_gdrive <- generate_interface_gdrive(server_id = "gdrive_test_server",
                                          task_dir =  "tasks_test",
                                          result_dir = "results_test",
                                          file_dir = "files_test",
                                          tmp_dir = "/home/owndir/tmp")
save(ifobj_gdrive, file = "ifobj_gdrive.RData")
```

## Starting a server
Next, after transmitting the RData file containing the interface object to the remote computer, start a server session using the commands in the next code chunk. For the first run, you may encounter a multi-step login process for Google Drive, which is necessary only at the first login. In later R sessions, `googledrive` will ask you to choose the account to be used. Upon success, you should see a blinking cursor in the R terminal, indicating that the server is running and waiting for incoming task packages

```r
load("ifobj_gdrive.RData")
RunServer(ifobj_gdrive)
```

## Sending tasks to and receiving results from the server
To send a task from the client side to the server, you have to first create a task package, including the R expression itself, as well as the necessary objects and libraries required for the evaluation of the command. This can be performed using the `PackIn` function. Such a task package can be sent to the remote server using the `SendOut` function. Make sure to store the output of the `PackIn` and `SendOut` functions in a variable, as it will be needed to fetch the results from Google Drive when the server finishes with the job. In the following code chunk I present a simple use case when I ask the remote server to filter the iris dataset using the `filter` function from the `dplyr` package.

```r
# loading the interface object on the client
load("test/ifobj_gdrive.RData")

# creating the task object and sending it to the server
data("iris")
taskobj <- filter(iris, Sepal.Length > 4 & Sepal.Length < 4.5) |>
  PackIn(objects = c("iris"), libraries = c("dplyr")) |>
  SendOut(ifobj_gdrive)

# attempting to get back results
GetBack(taskobj, interface = ifobj_gdrive)
```

The following screenshots show the process of sending a task to the server in RStudio:

![kép](https://github.com/user-attachments/assets/25e2a678-529c-4932-972c-e48de650b656)

And this is how getting the results back looks like:

![kép](https://github.com/user-attachments/assets/637a16e3-2849-4c0b-9918-fb1e6f36a555)
