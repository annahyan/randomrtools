#' Retrieve pathway gene list
#' 
#' The function retrieves pathway genes from KEGG and returns either as
#' entrez genes or gene symbols.
#' 
#' @param pathway pathway name or a keyword
#' @param gene_format output format: ENTREZ or SYMBOL. default: ENTREZ
#' 
#' @export

pathway_gene_list = function(pathway, gene_format = "ENTREZ") {
    
    keggid <- KEGGREST::keggFind("pathway", pathway)
    
    if (length(keggid) > 1) {
        cat(length(keggid), " pathways found matching the query:\n")
        notpicked = TRUE 
        while (notpicked) {
            for(i in seq_along(keggid)) {
                cat("\t", i,": ", names(keggid)[i], " - ", keggid[i], "\n")
            }
            index = readline(prompt = "Pick your poison :> ")
            index = as.numeric(trimws(index))
            
            if (! is.na(index) &  index <= length(keggid)) {
                keggid = keggid[index]
                notpicked = FALSE
            }
        }
    }
    
    hsa_keggid = gsub("map", "hsa", names(keggid))
    
    pathway_retrieve = KEGGREST::keggGet(hsa_keggid)
    
    gene_list = pathway_retrieve[[1]]$GENE
    
    
    if (gene_format == "SYMBOL") {
        gene_descriptons = gene_list[seq(2, length(gene_list), 2)]
        output = sapply(strsplit(gene_descriptons, ";"), function(x) x[1])
    } else {
        if (gene_format == "ENTREZ") { 
            output = gene_list[seq(1, length(gene_list), 2)]
            }
    }
    return(output)
}
