# DCE_MIR

Author : Zhonghui Feng


This repository contains materials for a discrete choice experiment (DCE) project on preferences for pesticide reduction policies.  

The project studies how the reference scenario used in a choice experiment may affect respondents’ evaluation of radical environmental transition policies. 

In particular, it compares different questionnaire versions in which pesticide reduction policies are evaluated relative to either the current situation or a full pesticide ban.

## Project overview

Environmental transitions often require policies that differ substantially from the current status quo. In agriculture, pesticide reduction involves trade-offs between environmental and health concerns, agricultural production constraints, and possible increases in food prices.

This project uses a split-sample discrete choice experiment to examine whether respondents evaluate the same policy trade-offs differently depending on the reference scenario presented in the choice task. 

Here is the link to the questionnaire : https://qualtricsxmghsvmdzb7.qualtrics.com/jfe/form/SV_3rali6IcCbHXrhA

The main research question is:

> How does the reference scenario affect preferences over radical pesticide reduction policies?

## Experimental design

Each choice card presents three options. The options are described by two attributes:

1. **Reduction in farmers’ pesticide use**
2. **Increase in the price of the weekly shopping basket of food**

The design uses the following fixed reference policies:

- **Current situation:** 0% pesticide reduction, €0 additional weekly food cost
- **Full ban:** 100% pesticide reduction, €6 additional weekly food cost

Intermediate tax policies are defined by:

- Pesticide reduction levels: 25%, 50%, 75%
- Additional weekly food cost: €1, €2, €3, €4, €5

## Split-sample structure

The questionnaire includes three sample versions:

### Sample 1: Status quo reference

Respondents evaluate policy alternatives relative to the current situation.

### Sample 2: Reference switch

For cards without a full-ban alternative, the reference option is changed to the full ban while the two policy alternatives remain unchanged.

For cards where the full ban is already included as an alternative, the current situation remains the reference option to avoid duplicate full-ban options.

### Sample 3: Mirrored design

The choice environment is symmetrically transformed around the midpoint:


r' = 100 - r

c' = 6 - c

## Choice cards
Choice cards

The repository includes visual choice cards used in the questionnaire.

File naming convention:

cardNumber_sampleNumber.png


## Questionnaire

The questionnaire is implemented in Qualtrics.

Because the free Qualtrics account imposes a 30-question limit, several choice cards may be grouped together in matrix-table format for the test version.

In the full version, each choice card can be displayed on a separate page if an institutional Qualtrics or LimeSurvey account is available.

The questionnaire contains:

1. Introduction
2. Socio-demographic questions
3. Food expenditure and organic food consumption questions
4. Information page
5. Choice tasks

Typical files in this repository include:

Info_Page_Pesticides_EN.png

1_1.png ... 12_1.png

1_2.png ... 12_2.png

1_3.png ... 12_3.png

R scripts for D-efficient design selection


## Notes

This repository is part of an ongoing research project. The current version is intended for pilot testing and supervisor review.
The design and questionnaire may be updated as the project develops.

## Reference 
Martinet, V., David, M., Mermet-Bijon, V., & Crastes dit Sourd, R. (2025). Cost vector effects in
forced-choice discrete choice experiments. Journal of Choice Modelling, 55.


