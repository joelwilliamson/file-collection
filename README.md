# file-collection
Provide a uniform interface over file archives and directories in Haskell

The interface is essentially the same as that provided by `directory`, except each function also take reference to the archive/root directory it is working under, and mutating operations return the structure wrapped in IO.
