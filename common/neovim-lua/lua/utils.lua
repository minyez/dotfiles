-- some utilies for configs, need to load first

-- Check if the operating system is UNIX-like by checking the path separator
function IS_UNIX()
  local sep = package.config:sub(1,1)
  if sep == "/" then
    return true
  end
  return false
end

-- Check if the operating system is macOS by search "/Applications" directory
function IS_MACOS()
  if IS_UNIX() then
    if IS_DIR("/Applications") then
      return true
    end
  end
  return false
end

-- Check if a file or directory exists in this path
-- from https://stackoverflow.com/questions/1340230/check-if-directory-exists-in-lua
function EXISTS(file)
   local ok, err, code = os.rename(file, file)
   if not ok then
      if code == 13 then
         -- Permission denied, but it exists
         return true
      end
   end
   return ok, err
end

-- Check if a directory exists in this path
-- from https://stackoverflow.com/questions/1340230/check-if-directory-exists-in-lua
function IS_DIR(path)
   -- "/" works on both Unix and Windows
   return EXISTS(path.."/")
end

-- check if current time is daytime.
-- st and et are the hours of staring and ending of daytime,
-- default to 6 and 18, respectively
function IS_DAYTIME(st, et)
  st = st or 6
  et = et or 18
  local time = os.date("*t")
  if time.hour > st and time.hour < et then
    return true
  end
  return false
end

-- some hand checks
-- print(IS_UNIX())
-- print(IS_MACOS())
-- print(IS_DAYTIME())
