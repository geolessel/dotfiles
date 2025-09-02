local M = {}

-- Smart file picker - git in git repos, rg elsewhere
function M.smart_file_picker()
  -- Check if we're in a git repository using vim.fs.find
  local git_root = vim.fs.find('.git', {
    upward = true,
    stop = vim.fs.dirname(vim.uv.os_homedir()),
    path = vim.fs.dirname(vim.api.nvim_buf_get_name(0))
  })

  if #git_root > 0 then
    -- In git repo, use git to show tracked files
    MiniPick.builtin.files({ tool = 'git' })
  else
    -- Not in git repo, use ripgrep
    MiniPick.builtin.files({ tool = 'rg' })
  end
end

return M