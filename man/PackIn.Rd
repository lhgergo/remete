\name{PackIn}
\alias{PackIn}
\title{Creates a task package on the client side that can be sent to the server.}
\usage{
PackIn(expr, objects = NULL, libraries = NULL, task_id = NULL, tmp_dir = "~/tmp/")
}
\arguments{
  \item{expr}{the command to be executed by the remote server, feeded in as an expression.)}
  \item{objects}{character; the names of objects to be sent in the task package, that is needed for the evaluation of the expression (e.g. input variables used)}
  \item{libraries}{character; the names of libraries required to evaluate the expression.)}
  \item{task_id}{character; a unique task identifier (not compulsory)}
  \item{tmp_dir}{character; path to the temporary directory.}
}

\description{The function creates a task package including the expression to be evaluated, as well as an environment containing the data objects and libraries required for performing the task.
}

\value{
A character vector of two elements; the first indicating the unique identifier of the task, the second showing the path to the task package file (RData) on the local computer. In parallel, the task package itself is saved in the temporary directory.
}

\examples{
# a simple use-case
taskobj <- PackIn(4+2)

# a more complex command - running HLA binding predictions
taskobj <- PackIn(emhace.tools::RunNetMHCpan_ng(alleles = "HLA-A02:01", peptides = peps_unq, version_number = "4.1",
                                                software_path = "/home/lhgergo/Programok/netMHCpan-4.1/netMHCpan"),
                  objects = "peps_unq", libraries = c("purrr", "emhace.tools", "magrittr"))

# same, but using R's built-in piping
taskobj <- emhace.tools::RunNetMHCpan_ng(alleles = "HLA-A02:01", peptides = peps_unq, version_number = "4.1",
                                         software_path = "/home/lhgergo/Programok/netMHCpan-4.1/netMHCpan") |>
  PackIn(objects = "peps_unq", libraries = c("purrr", "emhace.tools", "magrittr"))
}
