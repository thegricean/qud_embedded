# QUD effects on the interpretation of embedded quantifiers

The project is a follow-up on and combination of the following four papers:

- Chemla & Spector 2011
- Potts, Lassiter, Levy & Frank 2015
- Degen & Goodman 2014 
- Gotzner & Benz, 2018

Wee add Degen & Goodman style QUD manipulations to the Chemla and Potts style paradigms for assessing the derivation of local implicatures.

The question: does the QUD affect the probability of implicatures in sentences where "some" is embedded under "each", e.g. "Each girl found some of her marbles"?

## Design

- 60 participants per QuD condition

- Critical sentence: Each girl found some of her marbles

- 4 contextual QuD conditions, between-subject: all-all, any-all, all-any, any-any

- 4 pictorial/inference conditions, within-subject: false (ANNN), literal (AAAA), weak (AASS), strong (SSSS)

- Critical comparison: QuD effect on weak and literal condition

## Hypotheses

We test two competing hypotheses:

1. Embedded implicatures are computed locally by the grammar (blind to the QuD).

2. Embedded implicatures are dependent on the contextual QuD.

## Predictions 

1. If embedded implicatures are insensitive to QuD manipulations:
	- % TRUE respones: literal < weak in all QuD contexts

2. If there is an interaction between QuD and inference condition, in terms of % TRUE responses:
	- literal < weak in each-all QuD context
	- literal = weak in any-any QuD context
	- exploratory for any-all and all-any QuD context, but intermediate in comparison with each-all and any-any
	- Overall inference pattern in % TRUE responses: false < literal < weak < strong
	- Baseline: No effect of QuD manipulation on false and strong control conditions

## Analysis

Mixed effects logistic regression predicting TRUE responses from
- fixed effects: 
	- main effect: 2 inference conditions (weak, literal), with weak as reference level
	- main effect: 4 QuD conditions (allall, anyany, allany, anyall), with anyany as reference level
- random effects:
	- by-subject random intercepts 
	- no random slopes because between-subjects manipulation for QUD, and only one data point per inference condition per subject
	- no by-item random effects because there are no different items that aren't entirely confounded with inference condition