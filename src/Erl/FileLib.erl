-module(erl_fileLib@foreign).
-export([ensureDir_/1, fileSize/1, isDir/1, isFile/1, isRegular/1]).

ensureDir_(Name) -> fun () ->
  filelib:ensure_dir(Name)
end.

fileSize(Name) -> fun () ->
  filelib:file_size(Name)
end.

isDir(Name) -> fun () ->
  filelib:is_dir(Name)
end.

isFile(Name) -> fun() ->
  filelib:is_file(Name)
end.

isRegular(Name) -> fun() ->
  filelib:is_regular(Name)
end.

