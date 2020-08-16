.table_sorted <- function(semtype_vec) {
    semtypes <- stringr::str_split(semtype_vec, ",") %>% unlist()
    tab <- sort(table(semtypes), decreasing = TRUE)
    list(tab)
}

.table_sorted_from_list <- function(l) {
    result <- l %>% unlist() %>% table() %>% sort(decreasing = TRUE)
    list(result)
}

.unique_as_list <- function(x) {
    list(unique(x))
}
