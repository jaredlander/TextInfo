<!-- README.md is generated from README.Rmd. Please edit that file -->
Package for extracting entity information from text documents using [MITIE](https://github.com/mit-nlp/MITIE) and storing it as an edgelist.

Some functions here are taken directly from the [`openNLP`](http://cran.r-project.org/web/packages/openNLP/index.html) package. This was done, rather than using that package as a DEPENDS, because [`openNLP`](http://cran.r-project.org/web/packages/openNLP/index.html) depends on [`rJava`](http://cran.r-project.org/web/packages/rJava/index.html), a requirement we aim to avoid.

A number of examples call for a MITIE ner model. This was too big to store in this repo so please download from <http://sourceforge.net/projects/mitie/files/binaries/MITIE-models-v0.2.tar.bz2>.

To install first install MITIE following the instructions at <https://github.com/mit-nlp/MITIE/tree/master/tools/R-binding>.

Then install `TextNet` with `devtools::install_github('jaredlander/TextNet')`.
