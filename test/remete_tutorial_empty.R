# loading the library -----------
library(remete)

# creating an interface object -----------

# in the browser: let's check the folders the previous function call has created -----------

# in the remote computer: start the remote server -----------

# task: 40 + 2 on a remote server -----------

# task: perform predictions on a remote server -----------
aas_unq <- protr::AABLOSUM62 |> rownames()
peps_unq <- sapply(1:100, \(i) sample(aas_unq, size = 9) |> paste0(collapse = ""))
alleles <- "HLA-A02:01"

