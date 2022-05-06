###############################################################################
# Molly McDermott
# Created 5/6/22
# Experimental design for pretest-posttest survey analysis
##############################################################################

#### POWER ANALYSIS ####
# experimental design is paired
# one-way t-test. hypotheses:
# increase in science identity
# decrease in scientist stereotypes
# increase connectedness to nature
# increase climate change hope

## sample size - maximum 21, planning for 19
## alpha 0.05

library(pwr)

#power calculations
# what is the power to detect a small effect size?
pwr.t.test(n=19, d=0.10, sig.level=.05, alternative="greater")
#0.09

# what is the power to detect a medium effect size?
pwr.t.test(n=19, d=0.30, sig.level=.05, alternative="greater")
#0.23

# what is the power to detect a large effect size?
pwr.t.test(n=19, d=0.50, sig.level=.05, alternative="greater")
#0.45

#effect size calculations
# what is the effect size that can be detected with power of 0.80?
pwr.t.test(n=19, power=0.8, sig.level=.05, alternative="greater")
# 0.82
