plot_optimisation <- function(data,
                              kpi,
                              ncol,
                              max_value,
                              min_value) {

  data %>%
    dplyr::as_tibble() %>%
    dplyr::rename(kpi_value = Value) %>%
    tidyr::pivot_longer(-c(kpi_value, run_type)) %>%
    dplyr::filter(kpi_value <= max_value) %>%
    dplyr::filter(kpi_value >= min_value) %>%
    ggplot2::ggplot(ggplot2::aes(value, kpi_value)) +
    ggplot2::geom_point(ggplot2::aes(color = run_type)) +
    ggplot2::facet_wrap(~ name, ncol = ncol, scales = "free_x") +
    ggplot2::labs(title = stringr::str_glue("Optimisation plots for {kpi}"),
                  x = "", y = stringr::str_glue("{kpi}")) +
    ggplot2::theme_set(ggplot2::theme_light())
}
