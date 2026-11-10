-- rdap for nvim-dap: require("rdap").setup() (with this repository on 'runtimepath' and `rdap`
-- on $PATH, or setup({ command = "/path/to/rdap" })). For R scripts and documents (Rnw, R
-- Markdown, Quarto); a file in a package's tests/testthat runs with the package loaded.
local M = {}

-- the last script debugged, and the files it loaded (source()d), by real path
local last = { program = nil, files = {} }

local function realpath(path)
  return vim.uv.fs_realpath(path) or path
end

-- the R scripts (next to `file`, or in the working directory) that source() it
local function sourcing(file)
  local found, seen = {}, {}
  local dirs = { vim.fs.dirname(file), vim.fn.getcwd() }
  for _, dir in ipairs(dirs) do
    for _, script in ipairs(vim.fn.glob(dir .. "/*.[rR]", false, true)) do
      script = realpath(script)
      if script ~= file and not seen[script] then
        seen[script] = true
        local ok, lines = pcall(vim.fn.readfile, script)
        for _, line in ipairs(ok and lines or {}) do
          local arg = line:match("^[^#]*source%s*%(%s*[\"']([^\"']+)[\"']")
          if arg then
            -- relative to the script's directory (where it's debugged from) or the working directory
            for _, base in ipairs({ vim.fs.dirname(script), vim.fn.getcwd() }) do
              local path = arg:sub(1, 1) == "/" and arg or base .. "/" .. arg
              if realpath(vim.fs.normalize(path)) == file then
                table.insert(found, script)
                goto next_script
              end
            end
          end
        end
      end
      ::next_script::
    end
  end
  return found
end

-- The script to debug: the current file, unless it's a file that a script loads: the script that
-- was last debugged and loaded it, else a script that source()s it. (Stepping into a sourced
-- function leaves you in its file; debugging *that* would only define the function.)
local function program()
  local file = realpath(vim.api.nvim_buf_get_name(0))
  if last.program and last.files[file] then
    return last.program
  end
  if file:match("%.[Rrs]nw$") then -- a part of an Rnw document: its root ("% !TeX root = main.Rnw")
    local ok, lines = pcall(vim.fn.readfile, file, "", 10)
    for _, line in ipairs(ok and lines or {}) do
      local root = line:match("^%%%s*!%a+%s*root%s*=%s*(%S+)")
      if root then
        return realpath(root:sub(1, 1) == "/" and root or vim.fs.dirname(file) .. "/" .. root)
      end
    end
  end
  local scripts = sourcing(file)
  if #scripts == 0 then
    return file
  elseif #scripts == 1 then
    return scripts[1]
  end
  local co = coroutine.running()
  vim.ui.select(scripts, {
    prompt = vim.fs.basename(file) .. " is sourced by. Debug:",
    format_item = function(s) return vim.fn.fnamemodify(s, ":~:.") end,
  }, function(choice)
    coroutine.resume(co, choice or require("dap").ABORT)
  end)
  return coroutine.yield()
end

function M.setup(opts)
  opts = opts or {}
  local dap = require("dap")
  dap.adapters.r = { type = "executable", command = opts.command or "rdap" }
  local configurations = {
    { type = "r", request = "launch", name = "Debug script", program = program },
    { type = "r", request = "launch", name = "Debug this file", program = "${file}" },
  }
  -- R scripts, and documents (their R chunks run in order: Rnw, R Markdown, Quarto)
  for _, ft in ipairs({ "r", "rnoweb", "rmd", "quarto" }) do
    dap.configurations[ft] = configurations
  end
  dap.listeners.after.launch.rdap = function(session)
    if session.config.type == "r" then
      last = { program = realpath(session.config.program), files = {} }
    end
  end
  dap.listeners.after.event_loadedSource.rdap = function(session, body)
    local path = body.source and body.source.path
    if session.config.type == "r" and path and path ~= last.program then
      last.files[path] = true
    end
  end
end

return M
