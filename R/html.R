#' Display data in your browser
#'
#' This function loads a data element in your browser
#' for quick display.
#'
#' @importFrom tableHTML "%>%"
#' @param df Name of a data element in your working environment
#' @param type Simple (s) or beautified (b) output
#' @export
html <- function(df, type = NULL) {

    ## * Parse arguments
    if (length(type) == 0) {

        ## Convert data to data frame
        vkr_data <- as.data.frame(df)

        ## Count rows
        vkr_nrows <- nrow(vkr_data)

        ## Build HTML table
        vkr_data %>%
            tableHTML::tableHTML(
                           replace_NA = TRUE,
                           round = 3) %>%
            tableHTML::add_css_column(
                           css = list(
                               "color", "#4d4d4d",
                               "text-align", "center"
                           ),
                           columns = names(df)) %>%
            tableHTML::add_css_row(css = list("background-color", "#f2f2f2"),
                        rows = tableHTML::even(1:vkr_nrows + 1)) %>%
            tableHTML::add_css_row(css = list("background-color", "#e6f0ff"),
                                   rows = tableHTML::odd(2:vkr_nrows + 1))

    } else if (type == "b") {

        ## Temp file
        file <- tempfile(fileext = ".html")

        ## Create HTML
        kableExtra::kable(as.data.frame(df), align = "l") %>%
            kableExtra::kable_styling(
                bootstrap_options = c("striped", "condensed"),
                full_width = T,
                fixed_thead = T,
                position = "left",
                ) %>%
            kableExtra::column_spec(
                seq_len(ncol(as.data.frame(df))),
                border_left = T,
                border_right = T
            ) %>%
            kableExtra::save_kable(file = file, self_contained = T)

        ## View HTML
        browseURL(file)

    }

}
