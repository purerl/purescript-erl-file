-module(erl_file@foreign).
-export([writeFile_/2, readFile_/1, deleteFile_/1, listDir_/1, readFileInfo_/3, deleteDir_/1, deleteDirRecursive_/1]).

-include_lib("kernel/include/file.hrl").

writeFile_(Filename, Data) -> fun () -> 
   file:write_file(Filename, Data)
end.

readFile_(Filename) -> fun () ->
  file:read_file(Filename)
end.

deleteFile_(Filename) -> fun () ->
  file:delete(Filename)
end.

listDir_ (Dir) -> fun() ->
  Result = file:list_dir(Dir),
  case Result of
    {ok, Filenames} -> {ok, lists:map(fun (T) -> unicode:characters_to_binary(T) end, Filenames)};
    Error -> Error
  end
end.

readFileInfo_(AtomToFileType, AtomToFileAccess, Filename) -> fun() ->

  Result = file:read_file_info(Filename, [{time, posix}]),

  case Result of
    {ok, #file_info{ size = Size
                   , type = FileType
                   , access = Access
                   , atime = ATime
                   , mtime = MTime
                   , ctime = CTime
                   , mode = Mode
                   , links = Links
                   , major_device = MajorDevice
                   , minor_device = MinorDevice
                   , inode = INode
                   , uid = Uid
                   , gid = Gid }} ->

                    {ok, #{ size => Size
                          , type => AtomToFileType(FileType)
                          , access => AtomToFileAccess(Access)
                          , atime => ATime
                          , mtime => MTime
                          , ctime => CTime
                          , mode => Mode
                          , links => Links
                          , major_device => MajorDevice
                          , minor_device => MinorDevice
                          , inode => INode
                          , uid => Uid
                          , gid => Gid
                          }};
    Error -> Error
  end
end.

deleteDir_ (DirectoryName) -> fun() ->
  file:del_dir(DirectoryName)
end.

deleteDirRecursive_ (DirectoryName) -> fun() ->
  file:del_dir_r(DirectoryName)
end.