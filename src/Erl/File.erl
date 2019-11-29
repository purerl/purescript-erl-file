-module(erl_file@foreign).
-export([writeFile_/2, readFile_/1, deleteFile_/1]).

writeFile_(Filename, Data) -> fun () -> 
   file:write_file(Filename, Data)
end.

readFile_(Filename) -> fun () ->
  file:read_file(Filename)
end.

deleteFile_(Filename) -> fun () ->
  file:delete(Filename)
end.