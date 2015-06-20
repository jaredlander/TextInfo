context('Checking that sent_detect handles different senetence structures.')

simpleOne <- 'This is a first sentence.'
simpleMult <- c('This is one sentence.', 'This is another.', 'And now a question?', 'Now we exclaim!')
complexOne <- 'This is one sentence. This is another. And now a question? Now we exclaim!'

resSimp <- sent_detect(simpleOne)
resMult <- sent_detect(simpleMult)
resComp <- sent_detect(complexOne)

test_that('sent_detect returns the appropriate number of elements', {
    expect_equal(length(resSimp), 1)
    expect_equal(length(resMult), 4)
    expect_equal(length(resComp), 4)
})

test_that('sent_detect gets the extraction right', {
    expect_equal(resSimp, simpleOne)
    expect_equal(resMult, simpleMult)
    expect_equal(resComp, simpleMult)
})