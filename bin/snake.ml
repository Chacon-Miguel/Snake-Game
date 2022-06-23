open! Core
open! Snake_lib

let () =
  Command_unix.run Run.command;
  
  Core.never_returns (Async.Scheduler.go ())
;;
