open Core.Std

module Queue : Identifier = String
module Name  : Identifier = String

module Job_status = struct
  type job_running = Pending | Running
  type job_done    = Completed | Failed
  type t           = R of job_running | D of job_done
end

type t = { queue   : Queue.t
	 ; payload : string
	 }
