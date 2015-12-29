
Dear Professor Grimmer,

We really appreciate the constructive and timely feedback from you and the reviewers. 

1) As Reviewer 1 suggests, how does the model perform as the assumptions
about spill overs deviate?  How sensitive are the inferences to the
particular assumptions you can make?

In the paper we clarified that we are focusing on testing sharp null
hypotheses arising from one model of how an experimental treatment might
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

When we set about adding the four unit example to the different sections of the
paper, we realized that we were not enhancing intuition: the two test
statistics applied to an experiment of size 4 mainly reflected the fact that we
had very little data. Instead, we added a discussion of the SSR and KS test
statistics applied to a case with no interference and with very simple models
of effect. We also added a figure and discussion that tries to show a bit more
about how models of effects work in the context of sharp null hypothesis
testing. We hope that this helps with the intuition so that the use of the SSR
test statistic to an experiment on a social network with a complex model of
treatment propagation is easier to understand.


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

We have added a section in which we replicate the analysis in Coppock's 2014
*Journal of Experimental Political Science* article "Information Spillovers:
Another Look at Experimental Estimates of Legislator Responsiveness". 

# Response to Reviewer 1

We appreciate the positive review. In response to this review and the comments from the editor and other reviewer, we now evaluate the SSR test statistic for three non-interference models (the sharp null model, the constant additive effects model, and the constant multiplicative effects model) and one new interference model (the one used by Coppock in his *Journal of Experimental Political Science* article).  We did not systematically search for other spillover models from political science, but agree that such a survey of models and now, test statistics, would be useful in the future.

# Response to Reviewer 2

We appreciate the positive review. We added a section in an attempt to provide more intuition about test statistics, models of effects, and their interaction. Continuing the 4 unit example did not, in the end, clarify Fisher/Rosenbaum-style hypothesis testing as we would have hoped so we only use that example in the beginning to illustrate the idea that each unit in a study may have many more than two potential outcomes. We did add a section showing a case where the KS test has more power than the SSR test -- in this case we used a skewed outcome with no spillover rather than heterogeneous exposure to spillover in order to keep things as simple as possible.


