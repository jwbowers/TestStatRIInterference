
Dear Professor Grimmer,

We really appreciate the constructive and timely feedback from you and the reviewers. 

1) As Reviewer 1 suggests, how does the model perform as the assumptions
about spill overs deviate?  How sensitive are the inferences to the
particular assumptions you can make?

In the paper we clarified that we are focusing on testing sharp null
hypotheses arising from one toy model of how an experimental treatment might
propagate across a social network. We have added some discussion to the paper
highlighting the fact that the power of a test of a sharp null hypothesis
depends on *both* the model of causal effects *and* the test statistic (and
the effective sample size which itself depends on sample size and network
characteristics). We also note that, given randomization, any test statistic
ought to produce valid statistical inferences (i.e. tests that would reject
the truth no more than $100 \alpha \%$ of the time).

Since the point of the paper is to learn about test statistics,
we do not explore alternative models of causal effects. The original
paper (BFP) on this method compared the performance of tests when the data did
and did not arise from the causal model and we point to that paper for those
who would like to pursue those set of questions. We did add some discussion
about the power of the SSR test statistic against different kinds of causal
effects: the SSR test statistic ought to be particularly sensitive to changes
in mean differences (after all, recall that the coefficient on a dichotomous
variable in an least squares model with a continuous outcome that minimizes
the SSR is the difference of means) and ought to be less sensitive to other
differences.




2) Reviewer 2 asks you to carry forward your toy example with four units
through the entire analysis.  If this isn't possible, please provide the
simplest case that illuminate the core intuition of your method.

We have done this.


3) Reviewer 2 also asks you to provide intuition about when the KS-test
would outperform the Sum of Squared Residuals.  As part of a simulation or
analysis of a simple case, demonstrating when/why the KS test outperforms
will be useful for gaining intuition about why the SSR is a generally
superior method.

In the text we note that the SSR is particularly sensitive to differences in
means (or perhaps symmetric differences in tails that also produce a lot of
movement in the mean of one group relative to another group) while the KS-test
ought to have more power to detect asymmetric differences in the distributions
of the treated versus control groups --- for example, differences that change
variance without moving the means by stretching one tail of the distribution. 

4) In letters we have an emphasis that new methods matter when applied to
real examples.  Can you demonstrate that a case where the SSR will change
the inferences we make when applied to a real world example?

We have added a section in which we replicate the analysis in Coppock's 2014 Journal of Experimental Political Science article "Information Spillovers: Another Look at Experimental Estimates of Legislator Responsiveness". 


