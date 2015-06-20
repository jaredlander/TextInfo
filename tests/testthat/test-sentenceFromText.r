context('Checking that getSentencesFromFile builds the proper data.frame.')

theText <- c('This is one sentence in a single element.', 'This element will hold multiple sentences. This is the second. And this is the third and last.')

res <- getSentencesFromText(theText)

test_that('getSentencesFromText returns the appropriate number of rows and columns', {
    expect_equal(nrow(res), 4)
    expect_equal(ncol(res), 2)
})

test_that('getSentencesFromText extracts the correct sentences', {
    expect_equal(res, data_frame(Sentence=c('This is one sentence in a single element.', 
                                            'This element will hold multiple sentences.', 
                                            'This is the second.',
                                            'And this is the third and last.'),
                                 Number=1:4))
})