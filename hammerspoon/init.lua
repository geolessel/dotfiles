hs.hotkey.bind({"cmd", "alt", "ctrl"}, "W", function()
      -- hs.alert.show("Hello World")
      hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
end)

-- Move the window to the left half of the screen
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w / 2
      f.h = max.h
      win:setFrame(f)
end)

-- Move the window to the right half of the screen
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      local halfScreen = max.w / 2

      f.x = halfScreen + max.x
      f.y = max.y
      f.w = halfScreen
      f.h = max.h
      win:setFrame(f)
end)

-- Move the window to fill the screen
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Up", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()

      f.x = max.x
      f.y = max.y
      f.w = max.w
      f.h = max.h
      win:setFrame(f)
end)

-- Move the window, regardless of size, to center of screen
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Down", function()
      local win = hs.window.focusedWindow()
      local f = win:frame()
      local screen = win:screen()
      local max = screen:frame()
      local halfWidth = max.w / 2
      local halfHeight = max.h / 2

      f.x = halfWidth - (f.w / 2)
      f.y = halfHeight - (f.h / 2)
      win:setFrame(f)
end)

-- Resize the window with a grid overlay
hs.hotkey.bind({"cmd", "ctrl"}, "R", function()
      hs.grid.setGrid('6x4')
      hs.grid.show()
end)

-- Reload this config
-- hs.hotkey.bind({"cmd", "alt", "ctrl"}, "R", function()
--       hs.reload()
-- end)

-- Autoreload this config
function reloadConfig(files)
   doReload = false
   for _, file in pairs(files) do
      if file:sub(-4) == ".lua" then
         doReload = true
      end
   end

   if doReload then
      hs.reload()
   end
end
local myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()
hs.alert.show("Hammerspoon config reloaded ~/.hammerspoon/init.lua")

-- Menu bar item
-- http://www.hammerspoon.org/go/
--[[
local caffeine = hs.menubar.new()
function setCaffeineDisplay(state)
   if state then
      caffeine:setTitle("AWAKE")
   else
      caffeine:setTitle("SLEEPY")
   end
end
function caffeineClicked()
   setCaffeineDisplay(hs.ceffeinate.toggle("displayIdle"))
end
if caffeine then
   caffeine:setClickCallback(caffeineClicked)
   setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end
--]]

-- 13e942ae704e1d49b94ae6a4ae25badf4ec65026 github personal access token
hs.hotkey.bind({"cmd", "alt", "ctrl"}, ",", function()
      hs.http.asyncGet("https://api.github.com/user", {Authorization="token 13e942ae704e1d49b94ae6a4ae25badf4ec65026"}, function(status, body, headers)
                          hs.alert.show(body)
      end)
end)

local choices = {}
local chooser = hs.chooser.new(function(choice)
      if not choice then return end
      hs.alert.show(choice.text)
      choices = {}
end)
local timers = {lastCalled=nil, timer=nil}
chooser:queryChangedCallback(function(query)
      timers.lastCalled = hs.timer.localTime()
      if timers.timer then timers.timer.stop() end
      timers.timer = hs.timer.doAfter(1, function()
                                         hs.alert.show(query)
         table.insert(choices, {text=query, subText="Your search"})
         chooser:refreshChoicesCallback()
      end)
      timers.timer:start()
      -- local now = hs.timer.localTime()
      -- local timer = hs.timer.waitUntil(function()
      --       return now - lastQuery > 2
      -- end, function()
      --    table.insert(choices, {text=query, subText="Your search"})
      --    chooser:refreshChoicesCallback()
      -- end)
end)
chooser:choices(function()
      return choices 
end)
hs.hotkey.bind({"cmd", "alt", "ctrl"}, "G", function()
      chooser:show()
end)

--[[
{
  "total_count": 2,
  "incomplete_results": false,
  "items": [
    {
      "id": 70993578,
      "name": "ecto_paging",
      "full_name": "Nebo15/ecto_paging",
      "owner": {
        "login": "Nebo15",
        "id": 5442866,
        "avatar_url": "https://avatars.githubusercontent.com/u/5442866?v=3",
        "gravatar_id": "",
        "url": "https://api.github.com/users/Nebo15",
        "html_url": "https://github.com/Nebo15",
        "followers_url": "https://api.github.com/users/Nebo15/followers",
        "following_url": "https://api.github.com/users/Nebo15/following{/other_user}",
        "gists_url": "https://api.github.com/users/Nebo15/gists{/gist_id}",
        "starred_url": "https://api.github.com/users/Nebo15/starred{/owner}{/repo}",
        "subscriptions_url": "https://api.github.com/users/Nebo15/subscriptions",
        "organizations_url": "https://api.github.com/users/Nebo15/orgs",
        "repos_url": "https://api.github.com/users/Nebo15/repos",
        "events_url": "https://api.github.com/users/Nebo15/events{/privacy}",
        "received_events_url": "https://api.github.com/users/Nebo15/received_events",
        "type": "Organization",
        "site_admin": false
      },
      "private": false,
      "html_url": "https://github.com/Nebo15/ecto_paging",
      "description": "Cursor-based pagination for Ecto.",
      "fork": false,
      "url": "https://api.github.com/repos/Nebo15/ecto_paging",
      "forks_url": "https://api.github.com/repos/Nebo15/ecto_paging/forks",
      "keys_url": "https://api.github.com/repos/Nebo15/ecto_paging/keys{/key_id}",
      "collaborators_url": "https://api.github.com/repos/Nebo15/ecto_paging/collaborators{/collaborator}",
      "teams_url": "https://api.github.com/repos/Nebo15/ecto_paging/teams",
      "hooks_url": "https://api.github.com/repos/Nebo15/ecto_paging/hooks",
      "issue_events_url": "https://api.github.com/repos/Nebo15/ecto_paging/issues/events{/number}",
      "events_url": "https://api.github.com/repos/Nebo15/ecto_paging/events",
      "assignees_url": "https://api.github.com/repos/Nebo15/ecto_paging/assignees{/user}",
      "branches_url": "https://api.github.com/repos/Nebo15/ecto_paging/branches{/branch}",
      "tags_url": "https://api.github.com/repos/Nebo15/ecto_paging/tags",
      "blobs_url": "https://api.github.com/repos/Nebo15/ecto_paging/git/blobs{/sha}",
      "git_tags_url": "https://api.github.com/repos/Nebo15/ecto_paging/git/tags{/sha}",
      "git_refs_url": "https://api.github.com/repos/Nebo15/ecto_paging/git/refs{/sha}",
      "trees_url": "https://api.github.com/repos/Nebo15/ecto_paging/git/trees{/sha}",
      "statuses_url": "https://api.github.com/repos/Nebo15/ecto_paging/statuses/{sha}",
      "languages_url": "https://api.github.com/repos/Nebo15/ecto_paging/languages",
      "stargazers_url": "https://api.github.com/repos/Nebo15/ecto_paging/stargazers",
      "contributors_url": "https://api.github.com/repos/Nebo15/ecto_paging/contributors",
      "subscribers_url": "https://api.github.com/repos/Nebo15/ecto_paging/subscribers",
      "subscription_url": "https://api.github.com/repos/Nebo15/ecto_paging/subscription",
      "commits_url": "https://api.github.com/repos/Nebo15/ecto_paging/commits{/sha}",
      "git_commits_url": "https://api.github.com/repos/Nebo15/ecto_paging/git/commits{/sha}",
      "comments_url": "https://api.github.com/repos/Nebo15/ecto_paging/comments{/number}",
      "issue_comment_url": "https://api.github.com/repos/Nebo15/ecto_paging/issues/comments{/number}",
      "contents_url": "https://api.github.com/repos/Nebo15/ecto_paging/contents/{+path}",
      "compare_url": "https://api.github.com/repos/Nebo15/ecto_paging/compare/{base}...{head}",
      "merges_url": "https://api.github.com/repos/Nebo15/ecto_paging/merges",
      "archive_url": "https://api.github.com/repos/Nebo15/ecto_paging/{archive_format}{/ref}",
      "downloads_url": "https://api.github.com/repos/Nebo15/ecto_paging/downloads",
      "issues_url": "https://api.github.com/repos/Nebo15/ecto_paging/issues{/number}",
      "pulls_url": "https://api.github.com/repos/Nebo15/ecto_paging/pulls{/number}",
      "milestones_url": "https://api.github.com/repos/Nebo15/ecto_paging/milestones{/number}",
      "notifications_url": "https://api.github.com/repos/Nebo15/ecto_paging/notifications{?since,all,participating}",
      "labels_url": "https://api.github.com/repos/Nebo15/ecto_paging/labels{/name}",
      "releases_url": "https://api.github.com/repos/Nebo15/ecto_paging/releases{/id}",
      "deployments_url": "https://api.github.com/repos/Nebo15/ecto_paging/deployments",
      "created_at": "2016-10-15T14:45:30Z",
      "updated_at": "2016-12-27T12:33:56Z",
      "pushed_at": "2016-12-17T22:13:33Z",
      "git_url": "git://github.com/Nebo15/ecto_paging.git",
      "ssh_url": "git@github.com:Nebo15/ecto_paging.git",
      "clone_url": "https://github.com/Nebo15/ecto_paging.git",
      "svn_url": "https://github.com/Nebo15/ecto_paging",
      "homepage": null,
      "size": 53,
      "stargazers_count": 5,
      "watchers_count": 5,
      "language": "Elixir",
      "has_issues": true,
      "has_downloads": true,
      "has_wiki": false,
      "has_pages": false,
      "forks_count": 1,
      "mirror_url": null,
      "open_issues_count": 3,
      "forks": 1,
      "open_issues": 3,
      "watchers": 5,
      "default_branch": "master",
      "score": 4.279786
    },
    {
      "id": 62077617,
      "name": "ecto_cursor_pagination",
      "full_name": "bleacherreport/ecto_cursor_pagination",
      "owner": {
        "login": "bleacherreport",
        "id": 6364513,
        "avatar_url": "https://avatars.githubusercontent.com/u/6364513?v=3",
        "gravatar_id": "",
        "url": "https://api.github.com/users/bleacherreport",
        "html_url": "https://github.com/bleacherreport",
        "followers_url": "https://api.github.com/users/bleacherreport/followers",
        "following_url": "https://api.github.com/users/bleacherreport/following{/other_user}",
        "gists_url": "https://api.github.com/users/bleacherreport/gists{/gist_id}",
        "starred_url": "https://api.github.com/users/bleacherreport/starred{/owner}{/repo}",
        "subscriptions_url": "https://api.github.com/users/bleacherreport/subscriptions",
        "organizations_url": "https://api.github.com/users/bleacherreport/orgs",
        "repos_url": "https://api.github.com/users/bleacherreport/repos",
        "events_url": "https://api.github.com/users/bleacherreport/events{/privacy}",
        "received_events_url": "https://api.github.com/users/bleacherreport/received_events",
        "type": "Organization",
        "site_admin": false
      },
      "private": false,
      "html_url": "https://github.com/bleacherreport/ecto_cursor_pagination",
      "description": "Ecto Plugin for Cursor Pagination",
      "fork": false,
      "url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination",
      "forks_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/forks",
      "keys_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/keys{/key_id}",
      "collaborators_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/collaborators{/collaborator}",
      "teams_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/teams",
      "hooks_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/hooks",
      "issue_events_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/issues/events{/number}",
      "events_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/events",
      "assignees_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/assignees{/user}",
      "branches_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/branches{/branch}",
      "tags_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/tags",
      "blobs_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/git/blobs{/sha}",
      "git_tags_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/git/tags{/sha}",
      "git_refs_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/git/refs{/sha}",
      "trees_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/git/trees{/sha}",
      "statuses_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/statuses/{sha}",
      "languages_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/languages",
      "stargazers_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/stargazers",
      "contributors_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/contributors",
      "subscribers_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/subscribers",
      "subscription_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/subscription",
      "commits_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/commits{/sha}",
      "git_commits_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/git/commits{/sha}",
      "comments_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/comments{/number}",
      "issue_comment_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/issues/comments{/number}",
      "contents_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/contents/{+path}",
      "compare_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/compare/{base}...{head}",
      "merges_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/merges",
      "archive_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/{archive_format}{/ref}",
      "downloads_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/downloads",
      "issues_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/issues{/number}",
      "pulls_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/pulls{/number}",
      "milestones_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/milestones{/number}",
      "notifications_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/notifications{?since,all,participating}",
      "labels_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/labels{/name}",
      "releases_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/releases{/id}",
      "deployments_url": "https://api.github.com/repos/bleacherreport/ecto_cursor_pagination/deployments",
   
      "updated_at": "2017-01-15T14:58:56Z",
      "pushed_at": "2017-01-09T21:48:07Z",
      "git_url": "git://github.com/bleacherreport/ecto_cursor_pagination.git",
      "ssh_url": "git@github.com:bleacherreport/ecto_cursor_pagination.git",
      "clone_url": "https://github.com/bleacherreport/ecto_cursor_pagination.git",
      "svn_url": "https://github.com/bleacherreport/ecto_cursor_pagination",
      "homepage": "",
      "size": 18,
      "stargazers_count": 2,
      "watchers_count": 2,
      "language": "Elixir",
      "has_issues": true,
      "has_downloads": true,
      "has_wiki": true,
      "has_pages": false,
      "forks_count": 0,
      "mirror_url": null,
      "open_issues_count": 0,
      "forks": 0,
      "open_issues": 0,
      "watchers": 2,
      "default_branch": "master",
      "score": 3.1577396
    }
  ]
}
--]]
