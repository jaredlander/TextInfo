#' @title extractPairs
#' @description Given text, extract entities that cooccur in a sentence
#' @details Given text, extract entities that cooccur in a sentence.  Text can be stored in files or in a column in a tbl or database.
#' @aliases extractPairs
#' @export extractPairs
#' @rdname extractPairs
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom dplyr data_frame group_by do rename
#' @importFrom useful reclass
#' @param x Either a character vector with names of files or a tbl or database source holding the text.  In the latter cases the text is assumed to be in a column named Text and the grouping variable is assumed to be named File.
#' @param nerModel A ner model supplied by MITIE
#' @param \dots Further arguments
#' @return A tbl listing entity cooccurences along with the file name and the sentence number.
#' @examples 
#' \dontrun{
#' ner_model_path <- "mitie/MITIE-models/english/ner_model.dat"
#' ner <- NamedEntityExtractor$new(ner_model_path)
#' textFiles <- file.path('data', 'NYTimes', dir('data/NYTimes/'))
#' extractPairs(textFiles, ner)
#' 
#' textDB <- src_sqlite('data/TimesDB/Articles.sqlite3')
#' textTable <- tbl(textDB, 'Articles')
#' textTable
#' extractPairs(textTable, ner)
#' }
#' 
extractPairs <- function(x, nerModel, ...)
{
    UseMethod('extractPairs')
}

#' @title extractPairs.character
#' @export extractPairs.character
#' @export
#' @rdname extractPairs
extractPairs.character <- function(x, nerModel, ...)
{
    # create a data.frame listing the files
    result <- data_frame(File=x)
    # read into a tbl
    result %<>% group_by(File) %>% 
        do(data_frame(Text=paste(readLines(.$File), collapse=''))) 
    
    # call extractPairs to run the code in extractPairs.tbl
    extractPairs(result, nerModel, ...)
}

#for both data.base stored and tbl stored ' @title extractPairs.tbl ' @export extractPairs.tbl ' @rdname extract.Pairs
#' @title extractPairs.tbl
#' @export extractPairs.tbl
#' @export
#' @rdname extractPairs
#' @param toSub Named vector where the elements are the pattern and the names are the replacement values
#' @param requiredTerms A vector of terms that must be extracted if they exist
#' @param ignore.case Logical indicating if requiredTerms is not case sensitive
#' @importFrom useful moveToFront subVector
extractPairs.tbl <- function(x, nerModel, toSub=NULL, requiredTerms=NULL, ignore.case=TRUE, ...)
{
    dots <- 
    x %>% 
        # group by the file
        group_by(File) %>% 
        # replace special characters with their alternate meanings
        mutate(Text=subVector(Text, toSub=toSub)) %>%
        # do the pair extraction for each sentence in each file
        do(extractTextInfo(text=., nerModel=nerModel, requiredTerms=requiredTerms, ignore.case=ignore.case)) %>%
        # rename Number to SentenceNumber
        rename(SentenceNumber=Number) %>%
        # reorder the columns
        moveToFront(c('One', 'Two')) %>%
        # give it a good class
        reclass(value='pairedEntity')
}

#' @title extractPairs.OrientDB
#' @export extractPairs.OrientDB
#' @export
#' @importFrom httr content
#' @importFrom purrr flatten map reduce
#' @importFrom dplyr bind_rows
#' @param class The document DB class from which to query
#' @param id List of IDs to read, if \code{NULL} it pulls every record
#' @rdname extractPairs
extractPairs.OrientDB <- function(x, nerModel, id=NULL, class, ...)
{
    # copy x to db for easier coding
    db <- x
    
    idQuery <- sprintf("SELECT @rid from %s", class)
    
    # get a list of IDs
    if(is.null(id))
    {
        id <- OrientExpress::query(db, idQuery) %>% content %>% flatten %>% map(function(x) x$rid) %>% 
            flatten %>% sub(pattern="#", replacement="", x=.)
    }
    
    # for now read each ID one at a time
    # eventually write code to read a few at a time
    resultList <- lapply(id, queryAndExtract.OrientDB, db=db, nerModel=nerModel, ...)
    
    # combine into one data_frame
    resultList %>% reduce(bind_rows)
}

#' @title queryAndExtract.OrientDB
#' @description Extracts entities from an OrientDB entry
#' @details Queries the database and runs the result through \code{extractPairs.tbl}
#' @author Jared P. Lander
#' @rdname queryAndExtract.OrientDB
#' @param ID Record ID to query
#' @param db An OrientDB connection
#' @param nerModel A ner model supplied by MITIE
#' @param \dots Further arguments
#' @importFrom httr content
#' @importFrom purrr flatten keep
#' @return A tbl listing entity cooccurences along with the ID and the sentence number.
queryAndExtract.OrientDB <- function(ID, db, nerModel, ...)
{
    # extract text
    theText <- OrientExpress::query(db, query=sprintf('SELECT FROM %s', ID)) %>% content %>% 
        flatten %>% (function(x){ x$result$text}) %>% keep(function(x) x != "") %>% 
        paste(collapse=" ")
    
    # build a data.frame
    extractPairs(dplyr::data_frame(File=ID, Text=theText), nerModel, ...)
}

#' @title extractPairs.OrientDB
#' @export extractPairs.OrientDB
#' @export
#' @importFrom httr content
#' @importFrom purrr flatten map reduce
#' @importFrom dplyr bind_rows
#' @param class The document DB class from which to query
#' @param id List of IDs to read, if \code{NULL} it pulls every record
#' @rdname extractPairs
extractPairs.es_conn <- function(x, nerModel, id=NULL, class, ...)
{
    # copy x to db for easier coding
    db <- x
    
    idQuery <- sprintf("SELECT @rid from %s", class)
    
    # get a list of IDs
    if(is.null(id))
    {
        id <- OrientExpress::query(db, idQuery) %>% content %>% flatten %>% map(function(x) x$rid) %>% 
            flatten %>% sub(pattern="#", replacement="", x=.)
    }
    
    # for now read each ID one at a time
    # eventually write code to read a few at a time
    resultList <- lapply(id, queryAndExtract.OrientDB, db=db, nerModel=nerModel, ...)
    
    # combine into one data_frame
    resultList %>% reduce(bind_rows)
}