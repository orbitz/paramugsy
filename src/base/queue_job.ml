open Core_extended.Std

module Queue = struct
  type t      = string
  let compare = String.compare
end

module Name = struct
  type t      = string
  let compare = String.compare
end

module Job_status = struct
  type job_running = Pending | Running
  type job_done    = Completed | Failed
  type t           = R of job_running | D of job_done
end

type t = { queue   : Queue.t
	 ; payload : string
	 }
