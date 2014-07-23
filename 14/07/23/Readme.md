RNAseq qc stuff for Luisa
=========================

Commands used were:

    pairs join -t -s reads/Gymnodorididae_Paliolla_cooki_MVR13_626_S25_L1234_R*.fastq.gz > reads.ilfq
    seqqs -i -p read_before reads.ilfq
    scythe -a adaptors.fa  -p 0.05 -q sanger -o reads.scythe.ilfq_serial  reads.ilfq
    ~/prog/bio/forks/scythe/scythe_par -a adaptors.fa  -p 0.05 -q sanger -o reads.scythe.ilfq_par reads.ilfq
    seqqs -i -p reads_scythe reads.scythe.ilfq_par
    ~/prog/bio/forks/sickle/sickle pe -t sanger -c reads.scythe.ilfq_par -M reads.sickle.ilfq -l 40 -q 20
    pigz reads.*fq*

With sickle from najoshi's repo (at a9acb65):

    sickle  pe -c reads.scythe.ilfq_par -M reads.najoshi_sickle.fq -l 40 -q 20 -t sanger
    seqqs -q sanger -p najoshi -i reads.najoshi_sickle.fq
