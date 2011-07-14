#!/bin/bash -e

export PATH=/opt/paramugsy:$PATH

memory_monitor_all_loop.sh /mnt/paramugsy_memory > /mnt/paramugsy_memory.log 2>&1&

PID=$!

time paramugsy sge -seqs_per_mugsy 2 -template_file /opt/paramugsy/pm_qsub_template.sh -seq_list $1 -out_dir $2 -tmp_dir /mnt/ecoli_run/tmp -out_maf $2/run.maf 2>&1 | tee run.log

kill $PID
