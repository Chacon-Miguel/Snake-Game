open! Core
open! Snake_lib

let () =
  Run.run ();
  Core.never_returns (Async.Scheduler.go ())
;;
