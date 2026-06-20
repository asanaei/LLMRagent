# Extracted from test-workflow.R:105

# test -------------------------------------------------------------------------
wf <- agent_workflow("w") |>
    add_node("step1", function(state) { state$v <- (state$input %||% 0) + 1; state }) |>
    add_node("step2", function(state) { state$v <- state$v * 2; state }) |>
    add_edge("step1", "step2")
run <- run_workflow(wf, input = 10, quiet = TRUE)
rp <- replay_run(run, wf, verify = "strict", quiet = TRUE)
expect_true(all(rp$steps$replay_match %in% c(TRUE, NA)))
snaps <- sort(list.files(file.path(run$checkpoint_dir, "state"), full.names = TRUE))
bad <- readRDS(snaps[length(snaps)])
bad$v <- 999999
saveRDS(bad, snaps[length(snaps)])
expect_error(replay_run(run, wf, verify = "strict", quiet = TRUE),
               class = "llmragent_replay_mismatch")
