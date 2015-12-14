library(ggplot2)
library(plyr)
library(reshape2)
library(scales)
library(grid)

reads = read.csv('500k_reads.csv')
reads = melt(reads,
             id.vars=.(barcode),
             value.name = "readsassigned",
             variable.name = "program")
reads$readsassigned = reads$readsassigned / 1000
reads$program = relevel(reads$program, ref="axe")

p = ggplot(reads, aes(x=program, y=readsassigned, fill=barcode)) +
  geom_bar(position="dodge", stat="identity", colour='black', size=0.1) +
  scale_fill_grey(start = 0, end = .9) +
  ylab("Reads Assigned (k)") +
  xlab("Algorithm") +
  theme_bw(8) +
  theme(legend.key.size=unit(0.2, units='cm'),
        legend.margin=unit(0.05, units='cm'),
        legend.text=element_text(size=4))
pdf("500k_reads_assigned.pdf", width=3, height=2.25)
(p)
dev.off()

pdf("500k_reads_assigned_zoom.pdf", width=3, height=2.25)
(p + scale_y_continuous(limits=c(110,160), oob=rescale_none))
dev.off()


reads = read.csv('500k_gbs_reads.csv')
reads = melt(reads,
             id.vars=.(barcode),
             value.name = "readsassigned",
             variable.name = "program")
reads$readsassigned = reads$readsassigned / 1000
reads$program = relevel(reads$program, ref="axe")

p = ggplot(reads, aes(x=barcode, y=readsassigned, fill=program)) +
  geom_bar(position="dodge", stat="identity", colour='black', size=0.1) +
  scale_fill_grey(start = 0, end = .9) +
  ylab("Reads Assigned (k)") +
  xlab("Barcode") +
  theme_bw(8) +
  theme(legend.key.size=unit(0.2, units='cm'),
        legend.margin=unit(0.05, units='cm'),
        legend.text=element_text(size=4),
        axis.text.x = element_text(angle = 35, hjust=1))
pdf("gbs_reads_assigned.pdf", width=3, height=2.25)
(p)
dev.off()



runtime = read.csv('500k_runtime.csv')
runtime = runtime[runtime$time=='krps',]
runtime$time = NULL
runtime = melt(runtime,
             value.name = "krps",
             variable.name = "program")
runtime$program = relevel(runtime$program, ref="axe")




pdf("500k_rps.pdf", width=3, height=2.25)
ggplot(runtime, aes(x=program, y=krps)) +
  geom_bar(position="dodge", stat="identity", fill="black") +
  ylab("Reads per Second (k)") +
  xlab("Algorithm") +
  theme_bw(8) +
  theme(legend.key.size=unit(0.2, units='cm'),
        legend.margin=unit(0.05, units='cm'),
        legend.text=element_text(size=4))
dev.off()
