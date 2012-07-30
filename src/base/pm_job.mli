type genome = string

type pairwise = genome * genome

type job_tree =
  | Nil
  | Mugsy_profile of (job_tree * job_tree)
  | Mugsy of pairwise list
  | Fake_mugsy of genome

type t = { job_tree : job_tree
	 ; pairwise : pairwise list
	 }

val make_job : int -> genome list -> t
