#' Produces a contingency table from all provided columns
#' and outputs a kableExtra object for the table 
#' @import tidyverse
#' @export

kable_contingency = function(x) {
col1 = colnames(x)[1]
col2 = colnames(x)[2]
    x %>% group_by_all() %>% 
        tally () %>% 
        spread(key = 1, value = n) %>% 
        rename("\t" = all_of(col2)) %>% add_column(" " = c(col2, " "), .before = "\t") %>% 
        knitr::kable() %>% 
        kable_minimal(position = "left",full_width = F) %>% 
        add_header_above(c(" ", " ", col1, ""), escape = FALSE) %>% 
        column_spec(1:2, bold = TRUE)
}