#wrk_dir <- paste0("tests/testthat/tmp_docs")
wrk_dir <- test_path("tmp_docs")

test_that("docs_paradigm_page works", {
  # expect_output(docs_paradigm_page("ginger", "fmri", path = wrk_dir),
  #                "does not exist")
  #
  # expect_true(file.exists(
  #   paste0(wrk_dir, "/01-fmri-ginger.Rmd")
  # ))

  # This works locally but fails on travis
  # expect_true(file.exists(
  #   paste0(wrk_dir, "/bibtex/fmri-ginger.bib")
  # ))

  # docs_paradigm_page("ginger2", "fmri", bib = FALSE, path = wrk_dir)
  #
  # expect_true(file.exists(
  #   paste0(wrk_dir, "/02-fmri-ginger2.Rmd")
  # ))
  # expect_false(file.exists(
  #   paste0(wrk_dir, "/bibtex/fmri-ginger2.bib")
  # ))
})

unlink(wrk_dir,recursive = TRUE)
