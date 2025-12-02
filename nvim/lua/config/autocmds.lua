-- Auto-resize splits when window is resized
vim.api.nvim_create_autocmd("VimResized", {
  callback = function()
    vim.cmd("wincmd =")
  end,
})

-- Auto-create directories when writing files
vim.api.nvim_create_autocmd("BufWritePre", {
  callback = function(args)
    local dir = vim.fs.dirname(args.file)
    if vim.fn.isdirectory(dir) == 0 then
      local choice = vim.fn.confirm(
        string.format("Directory '%s' does not exist. Create it?", dir),
        "&Yes\n&No",
        1
      )
      if choice == 1 then
        vim.fn.mkdir(dir, "p")
        print("Created directory: " .. dir)
      else
        error("Write cancelled - directory does not exist")
      end
    end
  end,
})

-- When closing neogit, select the previous tab instead of next
vim.api.nvim_create_autocmd("BufWinLeave", {
  pattern = "Neogit*",
  callback = function()
    -- Switch to previous tab if available
    if vim.fn.tabpagenr() > 1 then
      vim.cmd("tabprevious")
    end
  end
})

-- Format on save
vim.api.nvim_create_autocmd("BufWritePre", {
  callback = function()
    vim.lsp.buf.format()
  end,
})

-- Detect LFE (Lisp Flavored Erlang) files
vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  pattern = "*.lfe",
  callback = function()
    vim.bo.filetype = "lfe"
  end,
})