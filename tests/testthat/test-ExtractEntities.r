context('Checking that entities are extracted from sentences.')

theText <- "A network of new super PACs said Wednesday that it had raised $31 million to support Senator Ted Cruz's presidential campaign, a sum that could upend expectations in the race for the Republican nomination and rewrite the political rule book for outside spending"
nullText <- "This has no value."

# ner_model_path <- "tests/data/ner_model.dat"
ner_model_path <- "../data/ner_model.dat"

check_ner <- function()
{
    if(!file.exists(ner_model_path))
    {
        skip('NER file not available')
    }
}

test_that('extractSentenceInfo returns the appropriate number of rows and columns', {
    check_ner()
    ner <- NamedEntityExtractor$new(ner_model_path)
    res <- extractSentenceInfo(theText, nerModel=ner)
    expect_equal(nrow(res), 3)
    expect_equal(ncol(res), 4)
    
    resNull <- extractSentenceInfo(nullText, nerModel=ner)
    expect_equal(dim(resNull), c(1, 4))
})

test_that('extractSentenceInfo returns the right class', {
    check_ner()
    ner <- NamedEntityExtractor$new(ner_model_path)
    res <- extractSentenceInfo(theText, nerModel=ner)
    expect_is(res, 'Entity')
    
    resNull <- extractSentenceInfo(nullText, nerModel=ner)
    expect_is(resNull, 'Entity')
})

test_that('extractSentenceInfo returns the correct entities', {
    check_ner()
    ner <- NamedEntityExtractor$new(ner_model_path)
    res <- extractSentenceInfo(theText, nerModel=ner)
    expect_equivalent(res, data_frame(Entity=c('PACs', 'Ted Cruz', 'Republican'),
                                      Type=c('ORGANIZATION', 'PERSON', 'MISC'),
                                      Start=c(6, 18, 35), Stop=c(6, 19, 35))
                      )
    
    resNull <- extractSentenceInfo(nullText, nerModel=ner)
    expect_equivalent(resNull, data_frame(Entity=NA, Type=NA, Start=NA, Stop=NA))
})

test_that('extractEntityInfo returns a 1 row by 4 column data.frame', {
    check_ner()
    ner <- NamedEntityExtractor$new(ner_model_path)
    tag_names <- ner$get_possible_ner_tags()
    tokens <- mitie_tokenize(theText)
    entities <- ner$extract_entities(tokens)
    res1 <- extractEntityInfo(entities[[1]], tokens, tag_names)
    res2 <- extractEntityInfo(entities[[2]], tokens, tag_names)
    res3 <- extractEntityInfo(entities[[3]], tokens, tag_names)
    
    expect_equal(dim(res1), c(1, 4))
    expect_equal(dim(res2), c(1, 4))
    expect_equal(dim(res3), c(1, 4))
    
    expect_is(res1, 'tbl')
    expect_is(res2, 'tbl')
    expect_is(res3, 'tbl')
})


test_that('extractEntityInfo gets the proper info from an entity object', {
    check_ner()
    ner <- NamedEntityExtractor$new(ner_model_path)
    tag_names <- ner$get_possible_ner_tags()
    tokens <- mitie_tokenize(theText)
    entities <- ner$extract_entities(tokens)
    res1 <- extractEntityInfo(entities[[1]], tokens, tag_names)
    res2 <- extractEntityInfo(entities[[2]], tokens, tag_names)
    res3 <- extractEntityInfo(entities[[3]], tokens, tag_names)
    
    expect_equivalent(res1, data_frame(Entity='PACs', Type='ORGANIZATION', Start=6, Stop=6))
    expect_equivalent(res2, data_frame(Entity='Ted Cruz', Type='PERSON', Start=18, Stop=19))
    expect_equivalent(res3, data_frame(Entity='Republican', Type='MISC', Start=35, Stop=35))
})


test_that('The situation is handled properly when there are no tokens of interest', {
    check_ner()
    ner <- NamedEntityExtractor$new(ner_model_path)
    tag_names <- ner$get_possible_ner_tags()
    tokens <- mitie_tokenize(nullText)
    entities <- ner$extract_entities(tokens)
    
    expect_null(extractEntity(entities, tokens, tag_names))
    expect_error(extractEntityInfo(entities, tokens, tag_names))
    
    nullRes <- extractSentenceInfo(nullText, ner)
    expect_equal(dim(nullRes), c(1, 4))
    expect_is(nullRes, 'Entity')
    expect_equivalent(nullRes, data_frame(Entity=NA, Type=NA, Start=NA, Stop=NA))
})