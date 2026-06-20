# Extracted from test-mcp.R:103

# prequel ----------------------------------------------------------------------
fake_transport <- function(state = new.env()) {
  state$search_schema <- list(type = "object",
                              properties = list(q = list(type = "string")))
  function(method, params) {
    switch(method,
      "tools/list" = list(tools = list(
        list(name = "search", description = "Search the docs.",
             inputSchema = state$search_schema,
             annotations = list(readOnlyHint = TRUE)),
        list(name = "send_message", description = "Send a message to a user.",
             inputSchema = list(type = "object",
                                properties = list(to = list(type = "string"),
                                                  body = list(type = "string"))),
             annotations = list(destructiveHint = TRUE)))),
      "tools/get" = list(inputSchema = state$search_schema),
      "tools/call" = list(content = list(list(type = "text",
        text = paste0("result for ", params$name)))),
      list())
  }
}

# test -------------------------------------------------------------------------
tr <- fake_transport()
tools <- mcp_tools(list(url = "x"), transport = tr, audit = TRUE)
search <- Find(function(t) t$name == "search", tools)
out <- search$fn(q = "hello")
meta <- attr(out, "mcp")
expect_true(nzchar(meta$arguments_hash))
expect_true(nzchar(meta$result_hash))
expect_true(meta$audited)
