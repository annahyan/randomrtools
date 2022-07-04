#' Retrieve genes from GO term in human
#' 
#' The function retrieves all genes from GO term and returns either as
#' entrez genes or gene symbols.
#' 
#' @param GOquery GO id or the name of GO
#' @param gene.format output format: ENTREZ or SYMBOL or ENSEMBL. default: SYMBOL
#' 
#' @export

retrieve_GO_gene_list = function(GOquery, gene.format = "SYMBOL") {

    require(GO.db)
    require(org.Hs.eg.db)
        
    ### Checking whether GOquery is a GO id or GO name
    if (startsWith(GOquery, "GO:")) {
        go.id = GOquery
    } else {
        go.id = AnnotationDbi::GOID( GOTERM[ Term(GOTERM) == GOquery])
    }

    entrez.genes = AnnotationDbi::get(go.id, org.Hs.egGO2ALLEGS)

    if (gene.format == "ENTREZ") {
        return(entrez.genes)
    } else if (gene.format == "SYMBOL") {

        gene.symbol = AnnotationDbi::select(org.Hs.eg.db,
                                            keys = entrez.genes, columns = c("SYMBOL"))
        return(gene.symbol$SYMBOL%>% unique() %>% na.omit())

    } else if (gene.format == "ENSEMBL") {

        gene.ensembl = AnnotationDbi::select(org.Hs.eg.db,
                                             keys = entrez.genes, columns = c("ENSEMBL"))
        return(gene.ensembl$ENSEMBL %>% unique() %>% na.omit())

    } else {
        stop("Gene Format should be either ENTREZ or SYMBOL or ENSEMBL.")
    }
}

