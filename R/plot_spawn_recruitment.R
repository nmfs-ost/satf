plot_spawn_recruitment <- function(
    dat = NULL,
    unit_label = "metric tons",
    end_year = NULL
) {
  rec <- dat |>
    dplyr::filter(label == "recruitment",
                  module_name == "TIME_SERIES" | module_name == "t.series",
                  !is.na(year),
                  is.na(fleet) | length(unique(fleet)) <= 1,
                  is.na(sex) | length(unique(sex)) <= 1,
                  is.na(area) | length(unique(area)) <= 1,
                  is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
                  year != "S/Rcurve" | year != "Init" | year != "Virg"
    ) |> # SS3 and BAM target module names
    dplyr::mutate(estimate = as.numeric(estimate),
                  year = as.numeric(year)) |>
    dplyr::rename(recruitment = estimate) |>
    dplyr::select(-c(module_name, label))
}
