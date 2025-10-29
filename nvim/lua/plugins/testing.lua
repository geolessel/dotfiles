-- Run tests for current Elixir test file
local function run_tests()
  local current_file = vim.fn.expand('%:p')
  local filename = vim.fn.expand('%:t')

  -- Check if current file is an Elixir test file
  if not filename:match('_test%.exs$') then
    print("Not an Elixir test file: " .. filename)
    return
  end

  -- Run mix test on current file
  local test_cmd = 'mix test ' .. vim.fn.shellescape(current_file)
  print("Running: " .. test_cmd)
  vim.cmd('botright split | terminal ' .. test_cmd)
end

-- Keymaps
vim.keymap.set("n", "<leader>tt", run_tests, { desc = "Run tests for current file" })
