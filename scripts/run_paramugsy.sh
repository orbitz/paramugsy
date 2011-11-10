#!/bin/bash -e

PARAMUGSY_INSTALL=/opt/paramugsy
SEQS_PER_MUGSY=2
NUCMER_CHUNK_SIZE=10
TEMPLATE_FILE=pm_qsub_template.sh
SEQ_LIST=
TMP_DIR=
OUT_MAF=


usage()
{
cat << EOF
usage: $0 options

This script run the test1 or test2 over a machine.

OPTIONS:
   -h      Show this message
   -s      Path to file list of sequences
   -o      Output MAF location
   -d      Temporary file directory, must be creatable on all machines in the run. MUST BE AN ABSOLUTE PATH.
   -m      Maximum number of sequences per Mugsy run (default $SEQS_PER_MUGSY)
   -n      Maximum number of nucmer searches to do per job (default $NUCMER_CHUNK_SIZE)
   -p      ParaMugsy install directory (default $PARAMUGSY_INSTALL)
EOF
}



while getopts “s:o:d:m:n:t:” OPTION
do
     case $OPTION in
         h)
             usage
             exit 1
             ;;
         s)
             SEQ_LIST=$OPTARG
             ;;
         o)
             OUT_MAF=$OPTARG
             ;;
         d)
             TMP_DIR=$OPTARG
             ;;
         m)
             SEQS_PER_MUGSY=$OPTARG
             ;;
	 n)
	     NUCMER_CHUNK_SIZE=$OPTARG
	     ;;
	 p)
	     PARAMUGSY_INSTALL=$OPTARG
	     ;;
         ?)
             usage
             exit
             ;;
     esac
done

if [[ -z $SEQ_LIST ]] || [[ -z $OUT_MAF ]] || [[ -z $TMP_DIR ]]
then
     usage
     exit 1
fi


export PATH=$PARAMUGSY_INSTALL:$PATH


paramugsy sge -seqs_per_mugsy $SEQS_PER_MUGSY -nucmer_chunk_size $NUCMER_CHUNK_SIZE -template_file $PARAMUGSY_INSTALL/pm_qsub_template.sh -seq_list $SEQ_LIST -out_dir $TMP_DIR -tmp_dir $TMP_DIR/tmp -out_maf $OUT_MAF
