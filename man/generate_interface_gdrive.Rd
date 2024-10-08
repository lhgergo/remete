\name{generate_interface_gdrive}
\alias{generate_interface_gdrive}
\title{Creates an "interface" function with pre-defined commands and parameters for communication between the client and the server via Google Drive.}
\usage{
generate_interface_gdrive(server_id, task_dir, result_dir, tmp_dir)
}
\arguments{
  \item{server_id}{a character-type value indicating the name of the server. Using unique names for each machine or user can ensure that tasks are assigned to the correct server.)}
  \item{task_dir}{a character-type value indicating the location of the directory to store task objects. The directory has to be reachable by both client and server.}
  \item{result_dir}{a character-type value indicating the location of the directory to store result objects. The directory has to be reachable by both client and server.}
  \item{file_dir}{a character-type value indicating the location of the directory to store files to be transmitted to the machine where the server is running. The directory has to be reachable by both client and server.}
  \item{tmp_dir}{a character-type value indicating the location of the temporary directory. This directory will be created locally on both client and server machine.}
}

\description{The function creates an "interface" function for communicating between the client and the server via Google Drive. For later usage, this output function of \code{generate_interface_gdrive} should be saved as an RData file, as it is both needed when starting the remote server and delegating tasks from the client.
Either upon first usage, or if Google Drive authentication is not saved, a login process might required via the \code{googledrive} package.

}

\value{
A function providing communication between a client and a specific server.
}

\examples{
ifobj_gdrive <- generate_interface_gdrive(server_id = "test_gdrive",
                                          task_dir =  "tasks_test",
                                          result_dir = "results_test",
                                          file_dir = "files_test",
                                          tmp_dir = "/home/owndir/tmp")
}
