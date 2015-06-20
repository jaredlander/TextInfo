context('Checking that entities are extracted from a vector of text.')

thisText <- c("A network of new super PACs said Wednesday that it had raised $31 million to support Senator Ted Cruz's presidential campaign, a sum that could upend expectations in the race for the Republican nomination and rewrite the political rule book for outside spending.",
              "The groups, four super PACs sharing variations of the name Keep the Promise, were established and secured commitments with virtually no warning over the course of several days beginning Monday.",
              "Dathan Voelter, an Austin, Tex., lawyer and friend of Mr. Cruz who is serving as treasurer for three of the super PACs, said the four organizations would operate in tandem, all seeking to help elect the Texas senator as president. Most of the contributions have already arrived, he said, and the remainder will be collected by the four groups by the end of the week.",
              "The dollar figures could not be independently verified, and none of the groups will need to file campaign disclosures with the Federal Election Commission until July. But an outside spending campaign of that size, combined with Mr. Cruz's demonstrated ability to pull in dollars from small donors, would substantially offset Mr. Cruz's difficulties in building a traditional network of regular large donors and volunteer fund-raisers, known as bundlers.",
              "The size of the contributions is likely to force backers of other candidates to rethink their budgets for the primary season; other super PACs lining up behind Republican candidates had planned to raise $20 million to $30 million over the course of the entire primary campaign.")

# ner_model_path <- "tests/data/ner_model.dat"
ner_model_path <- "../data/ner_model.dat"

check_ner <- function()
{
    if(!file.exists(ner_model_path))
    {
        skip('NER file not available')
    }
}


test_that('extractTextInfo returns the appropriate number of rows and columns and class', {
    check_ner()
    ner <- NamedEntityExtractor$new(ner_model_path)
    res <- extractTextInfo(thisText, nerModel=ner)
    
    expect_equal(dim(res), c(13, 3))
    expect_is(res, 'pairedEntity')
    expect_equivalent(res, data_frame(Number=c(1L, 1L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L), 
                                       One=c("PACs", "PACs", "Ted Cruz", NA, "Dathan Voelter", NA, "Cruz", NA, NA, NA, NA, NA, "PACs"), 
                                       Two = c("Ted Cruz", "Republican", "Republican", NA, "Austin", NA, "Texas", NA, NA, NA, NA, NA, "Republican")
                                       )
    )
})