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
        s = chnames[i]
        chnames[i] = paste(s, counts[s], sep = ".")
        counts[s] = counts[s] + 1
    }
    return(chnames)
}
