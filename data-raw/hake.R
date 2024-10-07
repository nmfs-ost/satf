# Big_Hake_std_res_2023.csv was downloaded from google drive
# and moved to the repository data-raw folder
hake <- utils::read.csv(
  file.path("data-raw", "hake.csv")
)

usethis::use_data(hake, overwrite = TRUE)
