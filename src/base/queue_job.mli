open Core_extended.Std

module Queue : sig
  type t = string
end

module Job_status : sig
  type job_running = Pending | Running
  type job_done    = Completed | Failed
  type t           = R of job_running | D of job_done
end

type t = { queue   : Queue.t
	 ; payload : string
	 }
