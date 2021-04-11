-- This file uses UTF8 encoding

user_menu_builder = nil
items = nil

-- system function that registers user functions that build and process menus
function register_menu_handler (user_func)
  user_menu_builder = user_func
end

-- system function that is called from C side to build the menu when user Right-Clicks file(s) in Explorer
function build_menu (filenames)
  -- Call user function to get list of menu items, filenames selected in Explorer are passed as argument list
  menu = user_menu_builder (filenames)

  -- Iterate menu items passing them to C function that really adds them to the menu
  items = {}
  menu_up = 0
  make_menu = function(menu)
    for i = 1,table.maxn(menu) do
      local item = menu[i]
      if item and item[1] then
        make_menu (item)    -- recursive call to handle menu items array

      elseif item and item.text then   -- skip empty menu items
        if item.submenu  then menu_down=1  else menu_down=0 end
        text = string.format (item.text, item.param)
        k = add_menu_item (text, menu_down, menu_up)
        items[k] = item
        menu_up = 0
        if item.submenu then
          make_menu (item.submenu)    -- recursive call to handle submenu
          menu_up = menu_up+1
        end
      end
    end
  end
  make_menu(menu)
end

-- function called from C to get help on menu item i
function get_help(i)
  return string.format (items[i].help, items[i].param)
end

-- function called from C to get command for menu item i
function get_command(i)
  return items[i].command
end


-- Auxiliary functions ---------------------------------------------

-- OS-dependent directory separator
DIR_SEPARATOR = package.config:sub(1,1)

-- Return filename directory: c:\dir\file.ext -> c:\dir
function get_dir(filename)
  return (string.match (filename, "(.*)"..DIR_SEPARATOR..".+"))
end

-- Return filename without directory: c:\dir\file.ext -> file.ext
function drop_dir(filename)
  return (string.match (filename, ".*"..DIR_SEPARATOR.."(.+)"))
end

-- Return extension: file.ext -> ext
-- Filename passed shouldn't contain path!
function get_ext(filename)
  return string.match (filename, ".+[.](.*)") or ""
end

-- Drop extension: file.ext -> file
-- Filename passed shouldn't contain path!
function drop_ext(filename)
  return string.match (filename, "(.+)[.].*") or filename
end

-- Add "" to string passed
function quote(filename)
  return "\""..filename.."\""
end

-- Concat arrays (that may contain nil values)
function concat(table1, table2)
  local t = {}
  local t1 = table1 or {}
  local t2 = table2 or {}
  local t1n = table.maxn(t1)
  local t2n = table.maxn(t2)
  for i = 1,t1n do
    t[i] = t1[i]
  end
  for i = 1,t2n do
    t[i+t1n] = t2[i]
  end
  return t
end

-- Concat associative tables
function append(table1, table2)
  local t = {}
  for i,v in pairs(table1 or {}) do
    t[i] = v
  end
  for i,v in pairs(table2 or {}) do
    t[i] = v
  end
  return t
end


-- FreeArc-specific functions -----------------------------------------

-- Detect archive type by its contents
function detect_archive_type(filename)
  -- Check that last 256 bytes of file contains sign of FreeArc footer block
  local data = read_from_file (filename, 2, -256, 256)
  if string.find (data, "ArC\1\8", 1, true) then
    return "FreeArc"
  elseif string.find (data, "PK\5\6", 1, true) then   -- or ZIP's end of central directory record
    return "zip"
  else
    return false
  end
end

-- Check for FreeArc SFX (legacy function)
function check_for_sfx(filename)
  return (detect_archive_type(filename)=="FreeArc")
end

-- Build multi-action FreeArc command
function multi_command(command, options, filenames)
  for i,f in ipairs(filenames) do
    if i>1 then command = command .. " ;" end
    command = command .. options .. quote(f)
  end
  return command
end
