-- This file uses UTF8 encoding

-- Maximum allowed length of command line
MAX_CMD_LENGTH = 4000


-- User-defined function that returns Right-Click Menu items for given filename(s)
--   filenames: array of UTF8-encoded filenames selected in Explorer
--   return value: array of menu items, each item is structure with the following fields:
--                   for commands: text, command, help
--                   for submenus: text, submenu, help (where submenu is, recursively, array of menu items)
register_menu_handler (function (filenames)
  path = get_dir(filenames[1])    -- directory where all selected items are located (since they can't belong to different directories)

  -- Special handling if only one item is selected
  if #filenames==1 then
    nameext     = drop_dir(filenames[1])
    basename    = drop_ext(nameext)
    ext         = string.lower(get_ext(nameext))
    isdir       = dir_exists(filenames[1])
    arcbase     = isdir and nameext or basename
    subdir      = basename..DIR_SEPARATOR
    add_options = ""   -- "-ep1": disabled due to bug in FreeArc
    filename    = quote(nameext)
  else
    isdir       = false
    arcbase     = drop_dir(path) or "default"
    subdir      = "*"..DIR_SEPARATOR
    add_options = ""
    t           = {}
    for i,f in ipairs(filenames) do
      t[i]      = quote(drop_dir(f))
    end
    filename    = table.concat(t," ")
  end
  if string.len(filename) > MAX_CMD_LENGTH   then filelist = "@{listfile}"  else filelist = filename end
  arcname = arcbase.."."..freearc_ext
  sfxname = arcbase.."."..freearc_sfx_ext
  zipname = arcbase..".zip"
  _7zname = arcbase..".7z"

  -- Check that all items selected are archives, SFX-es, non-FreeArc archives or at least files
  local all_arcs     = true    -- all selected items are .arc
  local all_sfxes    = true    -- all selected items are FreeArc SFXes
  local all_freearcs = true    -- all selected items are FreeArc archives
  local all_zips     = true    -- all selected items are non-FreeArc archives
  local all_archives = true    -- all selected items are archives
  local all_files    = true    -- all selected items are files (as opposite to directories)
  local freearc_all_ext_list = ' '..freearc_all_ext..' '
  local archives_ext_list    = ' '..archives_ext   ..' '
  local smart_menu_ext_list  = ' '..smart_menu_ext ..' '
  for _,f in ipairs(filenames) do
    if isdir or dir_exists(f) then
      all_arcs  = false
      all_sfxes = false
      all_zips  = false
      all_files = false
      break
    else
      local ext      = string.lower(get_ext(drop_dir(f)))
      local ext_elem = ' '..ext..' '
      local is_arc = string.find (freearc_all_ext_list, ext_elem, 1, true)
      local is_zip = string.find (archives_ext_list,    ext_elem, 1, true)
      local is_sfx = false
      if string.find (smart_menu_ext_list, ext_elem, 1, true) then
        local t = detect_archive_type(f)
        if t=="FreeArc" then
          if ext==freearc_sfx_ext   then is_sfx = true   else is_arc = true   end
        elseif t then
          is_zip = true
        end
      end
      all_arcs  = all_arcs  and is_arc
      all_sfxes = all_sfxes and is_sfx
      all_zips  = all_zips  and is_zip
    end
  end
  all_freearcs = all_arcs     or all_sfxes
  all_archives = all_freearcs or all_zips


  -- Compression commands
  compression_menu = {
    filename~=quote(arcname) and append (command.add2arc,  {param = arcname,  command = freearc.." a --noarcext "      ..add_options.." -sclUTF-8 -- " .. quote(arcname) .. " "  .. filelist}),
    filename~=quote(sfxname) and append (command.add2sfx,  {param = sfxname,  command = freearc.." a --noarcext -sfx " ..add_options.." -sclUTF-8 -- " .. quote(arcname) .. " "  .. filelist}),
    filename~=quote(zipname) and append (command.add2zip,  {param = zipname,  command = freearc.." a --noarcext -tzip "..add_options.." -sclUTF-8 -- " .. quote(zipname) .. " "  .. filelist}),
    filename~=quote(_7zname) and append (command.add2_7z,  {param = _7zname,  command = freearc.." a --noarcext -t7z  "..add_options.." -sclUTF-8 -- " .. quote(_7zname) .. " "  .. filelist}),
                                 append (command.add,      {                  command = freearc.." --add-dialog a "    ..add_options.." -- "..filename}),
  }

  -- Extraction commands
  extraction_menu = {
    #filenames==1   and append (command.open,         {command = freearc.." "..filename}),
                        append (command.extractTo,    {command = multi_command (freearc, " x -ad --noarcext -- ", filenames),  param = subdir}),
                        append (command.extractHere,  {command = multi_command (freearc, " x --noarcext -- ", filenames)}),
                        append (command.extract,      {command = freearc.." --extract-dialog x -- "..filename}),
                        append (command.test,         {command = multi_command (freearc, " t --noarcext -- ", filenames)}),
  }

  -- Modification commands
  modification_menu = {
      all_arcs      and append (command.arc2sfx,      {command = multi_command (freearc, " s --noarcext -- ",  filenames)}),
      all_sfxes     and append (command.sfx2arc,      {command = multi_command (freearc, " s- --noarcext -- ", filenames)}),
                        append (command.modify,       {command = freearc.." --add-dialog ch -- "..filename}),
      #filenames>1  and append (command.join,         {command = freearc.." --add-dialog j -- "..filename}),
  }

  -- Archive conversion commands
  cvt_menu = {
                        append (command.zip2arc,      {command = all2arc..                 " -- "..filename}),
                        append (command.zip2sfx,      {command = all2arc..            " -sfx -- "..filename}),
                        append (command.zip2a,        {command = freearc.." --add-dialog cvt -- "..filename}),
  }

  -- Provide various menus depending on types of files (archives) selected
  if all_freearcs then
    menu = concat (extraction_menu, concat (modification_menu, compression_menu))
  elseif all_zips then
    menu = concat (extraction_menu, concat (cvt_menu, compression_menu))
  elseif all_archives then
    menu = concat (extraction_menu, compression_menu)
  else
    menu = concat (compression_menu,  all_files and {append (command.anyfile, {submenu = concat(extraction_menu, concat(cvt_menu,modification_menu))})})
  end

  -- Make menu cascaded only if it isn't empty
  if cascaded then
    for i = 1,table.maxn(menu) do
      local item = menu[i]
      if item and (item[1] or item.text) then
        menu = { {text = FreeArcName,  submenu = menu,  help = FreeArcName.." commands"} }
        break
      end
    end
  end

  return menu
end)
