#' Make names for df
#' 
#' Similar function to base make.names, but starts from .1
#' 
#' @param chnames vector of strings-to-be-namse
#' 
#' @export


make_names_AH = function(chnames) {
    uniques = unique(chnames)
    counts = setNames(rep(1, times = length(uniques)), uniques)
    for (i in seq_along(chnames)) {
        chnames[i] = paste(chnames[i], counts[chnames[i]], sep = ".")
        counts[chnames[i]] = counts[chnames[i]] + 1
    }
    return(chnames)
}
